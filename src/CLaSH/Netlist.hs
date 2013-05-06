module CLaSH.Netlist where

import           Control.Lens            ((.=),_2)
import qualified Control.Lens            as Lens
import qualified Control.Monad           as Monad
import           Control.Monad.State     (runStateT)
import           Control.Monad.Writer    (runWriterT,listen)
import qualified Data.ByteString.Lazy.Char8 as LZ
import           Data.Either             (partitionEithers)
import           Data.HashMap.Lazy       (HashMap)
import qualified Data.HashMap.Lazy       as HashMap
import           Data.List               (elemIndex,nub)
import           Data.Maybe              (fromMaybe)
import qualified Data.Text.Lazy          as Text
import           Unbound.LocallyNameless (Embed(..),name2String,string2Name,runFreshMT,unembed,unbind,unrebind)

import CLaSH.Core.DataCon   (DataCon(..))
import CLaSH.Core.Literal   (Literal(..))
import CLaSH.Core.Pretty    (showDoc)
import CLaSH.Core.Prim      (Prim(..))
import qualified CLaSH.Core.Term as Core
import CLaSH.Core.Term      (Pat(..),Term(..),TmName)
import CLaSH.Core.Type      (Type)
import CLaSH.Core.Util      (collectArgs,isVar,termType)
import CLaSH.Core.Var       (Id,Var(..))
import CLaSH.Netlist.BlackBox
import CLaSH.Netlist.Id
import CLaSH.Netlist.Types as HW
import CLaSH.Netlist.Util
import CLaSH.Normalize.Util
import CLaSH.Primitives.Types as P
import CLaSH.Util

genNetlist ::
  HashMap TmName (Type,Term)
  -> PrimMap
  -> TmName
  -> IO ([Component],VHDLState)
genNetlist globals primMap topEntity = do
  (_,s) <- runNetlistMonad globals primMap $ genComponent topEntity Nothing
  return $ (HashMap.elems $ _components s, _vhdlMState s)

runNetlistMonad ::
  HashMap TmName (Type,Term)
  -> PrimMap
  -> NetlistMonad a
  -> IO (a,NetlistState)
runNetlistMonad s p
  = runFreshMT
  . (flip runStateT) s'
  . (fmap fst . runWriterT)
  . runNetlist
  where
    s' = NetlistState s HashMap.empty 0 0 HashMap.empty p (0,Text.empty,HashMap.empty)

genComponent ::
  TmName
  -> Maybe Integer
  -> NetlistMonad Component
genComponent compName mStart = do
  compExprM <- fmap (HashMap.lookup compName) $ Lens.use bindings
  case compExprM of
    Nothing -> error $ $(curLoc) ++ "No normalized expression found for: " ++ show compName
    Just (_,expr) -> makeCached compName components $
                      genComponent' compName expr mStart

genComponent' ::
  TmName
  -> Term
  -> Maybe Integer
  -> NetlistMonad Component
genComponent' compName componentExpr mStart = do
  varCount .= fromMaybe 0 mStart
  componentNumber <- cmpCount <%= (+1)

  let componentName' = (`Text.append` (Text.pack $ show componentNumber))
                     . ifThenElse Text.null
                          (`Text.append` (Text.pack "Component_"))
                          (`Text.append` (Text.pack "_"))
                     . mkBasicId
                     . Text.pack
                     $ name2String compName

  (vhdlMState . _2) .= componentName'

  (arguments,binders,result) <- splitNormalized componentExpr >>=
                                mkUniqueNormalized

  let ids = HashMap.fromList
          $ map (\(Id v (Embed t)) -> (v,t))
          $ arguments ++ (map fst binders)

  gamma <- (ids `HashMap.union`) . (HashMap.map fst)
           <$> Lens.use bindings

  varEnv .= gamma

  let resType  = coreTypeToHWType_fail $ ids HashMap.! result
  let argTypes = map (\(Id _ (Embed t)) -> coreTypeToHWType_fail t) arguments

  let netDecls = map (\(id_,_) ->
                        NetDecl (mkBasicId . Text.pack . name2String $ varName id_)
                                (coreTypeToHWType_fail . unembed $ varType id_)
                                Nothing
                     ) $ filter ((/= result) . varName . fst) binders
  (decls,clks) <- listen $ concat <$> mapM (uncurry mkConcSm . second unembed) binders

  let compInps       = zip (map (mkBasicId . Text.pack . name2String . varName) arguments) argTypes
  let compOutp       = (mkBasicId . Text.pack $ name2String result, resType)
  let component      = Component componentName' (nub clks) compInps compOutp (netDecls ++ decls)
  return component

mkConcSm ::
  Id
  -> Term
  -> NetlistMonad [Declaration]
mkConcSm bndr (Var _ v) = mkApplication bndr v []

mkConcSm bndr (Core.Literal lit) = do
  let dstId = mkBasicId . Text.pack . name2String $ varName bndr
  let bndrHWType = coreTypeToHWType_fail . unembed $ varType bndr
  let i = case lit of
            (IntegerLiteral i') -> i'
            _ -> error $ $(curLoc) ++ "not an integer literal"
  return [Assignment dstId (HW.Literal Nothing . NumLit $ fromInteger i)
         ]

mkConcSm bndr (Case (Var scrutTy scrutNm) _ [alt]) = do
  (pat,Var varTy varTm)  <- unbind alt
  let dstId = mkBasicId . Text.pack . name2String $ varName bndr
  let altVarId = mkBasicId . Text.pack $ name2String varTm
  let selId = mkBasicId . Text.pack $ name2String scrutNm
  let modifier = case pat of
                DataPat (Embed dc) ids -> let (_,tms) = unrebind ids
                                          in case (elemIndex (Id varTm (Embed varTy)) tms) of
                                               Nothing -> Nothing
                                               Just fI -> Just (Indexed (coreTypeToHWType_fail scrutTy,dcTag dc - 1,fI))
  let extractExpr = Identifier (maybe altVarId (const selId) modifier) modifier
  return [Assignment dstId extractExpr]

mkConcSm bndr (Case scrut@(Var scrutTy scrutNm) _ alts) = do
  alts' <- mapM unbind alts
  exprs <- mapM mkCondExpr alts'
  let dstId = mkBasicId . Text.pack . name2String $ varName bndr
  return [CondAssignment dstId (reverse exprs)]
  where
    mkCondExpr :: (Pat,Term) -> NetlistMonad (Expr,Expr,Expr)
    mkCondExpr (pat,alt) = do
      altTy    <- termType alt
      altAssgn <- mkConcSm (Id (string2Name "_ERROR_") (Embed altTy)) alt
      let altExpr = case altAssgn of
                      [Assignment _ e] -> e
                      [HW.BlackBox t]  -> let t' = Text.init . snd . Text.breakOnEnd (Text.pack " <= ") $ t
                                          in Identifier t' Nothing
                      _                -> error $ $(curLoc) ++ "Alt lead to more than one assignment: " ++ show altAssgn
      case pat of
        DefaultPat           -> return (Empty,Empty,altExpr)
        DataPat (Embed dc) _ -> let scrutId  = mkBasicId . Text.pack $ name2String scrutNm
                                    scrutHTy = coreTypeToHWType_fail scrutTy
                                in return (Identifier scrutId (Just (DC (scrutHTy,dcTag dc - 1))),dcToLiteral scrutHTy (dcTag dc),altExpr)
        LitPat  (Embed (IntegerLiteral i)) -> return (varToExpr scrut, HW.Literal Nothing (NumLit $ fromInteger i),altExpr)
        _                    -> error $ $(curLoc) ++ "Not an integer literal in LitPat"

    dcToLiteral :: HWType -> Int -> Expr
    dcToLiteral Bool 1 = HW.Literal Nothing (BoolLit True)
    dcToLiteral Bool 2 = HW.Literal Nothing (BoolLit False)
    dcToLiteral Bit 1  = HW.Literal Nothing (BitLit H)
    dcToLiteral Bit 2  = HW.Literal Nothing (BitLit L)
    dcToLiteral t i    = HW.Literal (Just $ conSize t) (NumLit (i-1))

mkConcSm bndr app = do
  let (appF,(args,tyArgs)) = second partitionEithers $ collectArgs app
  args' <- Monad.filterM (fmap representableType . termType) args
  case appF of
    Var _ f
      | all isVar args' && null tyArgs -> mkApplication bndr f args'
      | otherwise                      -> error $ $(curLoc) ++ "Not in normal form: Var-application with non-Var arguments"
    Data _ dc
      | all (\e -> isConstant e || isVar e) args' -> let dstId = mkBasicId . Text.pack . name2String $ varName bndr
                                                         hwTy  = coreTypeToHWType_fail . unembed $ varType bndr
                                                     in fmap ((:[]) . Assignment dstId) $ mkDcApplication hwTy dc args'
      | otherwise                                 -> error $ $(curLoc) ++ "Not in normal form: DataCon-application with non-Simple arguments"
    Prim (PrimFun nm _) -> do
      bbM <- fmap (HashMap.lookup . LZ.pack $ name2String nm) $ Lens.use primitives
      case bbM of
        Just p@(P.BlackBox {}) -> do
          bbCtx <- mkBlackBoxContext bndr args
          mkBlackBoxDecl (template p) bbCtx
        _ -> error $ $(curLoc) ++ "No blackbox found: " ++ name2String nm
    _ -> error $ $(curLoc) ++ "Not in normal form: application of a Let/Lam/Case: " ++ showDoc app

mkApplication ::
  Id
  -> TmName
  -> [Term]
  -> NetlistMonad [Declaration]
mkApplication dst fun args = do
  normalized <- Lens.use bindings
  case HashMap.lookup fun normalized of
    Just _ -> do
      vCnt <- Lens.use varCount
      vEnv <- Lens.use varEnv
      cN   <- Lens.use (vhdlMState . _2)
      (Component compName hidden compInps compOutp _) <- genComponent fun Nothing
      varCount .= vCnt
      varEnv .= vEnv
      (vhdlMState . _2) .= cN
      case length args == length compInps of
        True  -> do
          let dstId = mkBasicId . Text.pack . name2String $ varName dst
          let args' = map varToExpr args
          let hiddenAssigns = map (\(i,_) -> (i,Identifier i Nothing)) hidden
          let inpAssigns = zip (map fst compInps) args'
          let outpAssign = (fst compOutp,Identifier dstId Nothing)
          let instDecl = InstDecl compName dstId (outpAssign:hiddenAssigns ++ inpAssigns)
          return [instDecl]
        False -> error $ $(curLoc) ++ "under-applied normalized function"
    Nothing -> case args of
      [] -> do
        let dstId     = mkBasicId . Text.pack . name2String $ varName dst
        return [Assignment dstId (Identifier (mkBasicId . Text.pack $ name2String fun) Nothing)]
      _ -> error $ $(curLoc) ++ "Unknown function"

mkDcApplication ::
  HWType
  -> DataCon
  -> [Term]
  -> NetlistMonad Expr
mkDcApplication dstHType dc args = do
  argTys       <- mapM termType args
  let args'    = filter (not . isEmptyType . coreTypeToHWType_fail . snd) $ zip args argTys
  assngs       <- mapM (\(e,t) -> mkConcSm (Id (string2Name "_ERROR_") (Embed t)) e) args'
  let argExprs = map (\d -> case d of
                        [Assignment _ e] -> e
                        [HW.BlackBox t]  -> let t' = Text.init . snd . Text.breakOnEnd (Text.pack " <= ") $ t
                                            in Identifier t' Nothing
                        _                -> error $ $(curLoc) ++ "Datacon arguments lead to more than one assignment: " ++ show d
                     ) assngs

  case dstHType of
    SP _ dcArgPairs -> do
      let dcNameBS = Text.pack . name2String $ dcName dc
      let dcI      = dcTag dc - 1 -- fromMaybe (error $ $(curLoc) ++ "SP: dc not found") $ elemIndex dcNameBS $ map fst dcArgPairs
      let dcArgs   = snd $ indexNote ($(curLoc) ++ "No DC with tag: " ++ show dcI) dcArgPairs dcI
      case (compare (length dcArgs) (length argExprs)) of
        EQ -> return (HW.DataCon dstHType (Just $ DC (dstHType,dcI)) argExprs)
        LT -> error $ $(curLoc) ++ "Over-applied constructor"
        GT -> error $ $(curLoc) ++ "Under-applied constructor"
    Product _ dcArgs -> do
      case (compare (length dcArgs) (length argExprs)) of
        EQ -> return (HW.DataCon dstHType (Just $ DC (dstHType,0)) argExprs)
        LT -> error $ $(curLoc) ++ "Over-applied constructor"
        GT -> error $ $(curLoc) ++ "Under-applied constructor"
    Sum _ dcs -> do
      let dcNameBS = Text.pack . name2String $ dcName dc
      let dcI = fromMaybe (error "Sum: dc not found") $ elemIndex dcNameBS dcs
      return (HW.DataCon dstHType (Just $ DC (dstHType,dcI)) [])
    Bool -> do
      let dc' = case (name2String $ dcName dc) of
                 "True"  -> HW.Literal Nothing (BoolLit True)
                 "False" -> HW.Literal Nothing (BoolLit False)
                 _ -> error $ $(curLoc) ++ "unknown bool literal: " ++ show dc
      return dc'
    Bit -> do
      let dc' = case (name2String $ dcName dc) of
                 "H" -> HW.Literal Nothing (BitLit H)
                 "L" -> HW.Literal Nothing (BitLit L)
                 _ -> error $ $(curLoc) ++ "unknown bit literal: " ++ show dc
      return dc'
    Integer -> do
      let dc' = case (name2String $ dcName dc) of
                  "S#" -> Nothing
                  _    -> error $ $(curLoc) ++ "not a simple integer: " ++ show dc
      return (HW.DataCon dstHType dc' argExprs)
    Vector 0 _ -> return (HW.DataCon dstHType Nothing          [])
    Vector 1 _ -> return (HW.DataCon dstHType (Just VecAppend) [(head argExprs)])
    Vector _ _ -> return (HW.DataCon dstHType (Just VecAppend) argExprs)

    _ -> error $ $(curLoc) ++ "mkDcApplication undefined: " ++ show dstHType
