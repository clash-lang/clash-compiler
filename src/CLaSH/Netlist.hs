module CLaSH.Netlist where

import qualified Control.Monad           as Monad
import           Control.Monad.State     (runStateT)
import qualified Data.ByteString.Lazy.Char8 as LZ
import           Data.Either             (partitionEithers)
import           Data.HashMap.Lazy       (HashMap)
import qualified Data.HashMap.Lazy       as HashMap
import qualified Data.Label.PureM        as LabelM
import           Data.List               (elemIndex)
import           Data.Maybe              (fromMaybe)
import           Data.Text.Lazy          (pack)
import           Unbound.LocallyNameless (Embed(..),name2String,runFreshMT,unembed)

import CLaSH.Core.DataCon   (DataCon(..))
import CLaSH.Core.Literal   (Literal(..))
import CLaSH.Core.Prim      (Prim(..))
import qualified CLaSH.Core.Term as Core
import CLaSH.Core.Term      (Term(..),TmName)
import CLaSH.Core.Type      (Type)
import CLaSH.Core.Util      (collectArgs,isVar,termType)
import CLaSH.Core.Var       (Id,Var(..))
import CLaSH.Netlist.BlackBox
import CLaSH.Netlist.Types as HW
import CLaSH.Netlist.Util
import CLaSH.Netlist.VHDL   (mkVHDLBasicId)
import CLaSH.Primitives.Types as P
import CLaSH.Util

genNetlist ::
  HashMap TmName (Type,Term)
  -> PrimMap
  -> TmName
  -> IO [Component]
genNetlist globals primMap topEntity = do
  (_,s) <- runNetlistMonad globals primMap $ genComponent topEntity Nothing
  return $ HashMap.elems $ _components s

runNetlistMonad ::
  HashMap TmName (Type,Term)
  -> PrimMap
  -> NetlistMonad a
  -> IO (a,NetlistState)
runNetlistMonad s p
  = runFreshMT
  . (flip runStateT) s'
  . runNetlist
  where
    s' = NetlistState s HashMap.empty 0 0 HashMap.empty p

genComponent ::
  TmName
  -> Maybe Integer
  -> NetlistMonad Component
genComponent compName mStart = do
  compExprM <- fmap (HashMap.lookup compName) $ LabelM.gets bindings
  case compExprM of
    Nothing -> error "no normalized expression found"
    Just (_,expr) -> makeCached compName components $
                      genComponent' compName expr mStart

genComponent' ::
  TmName
  -> Term
  -> Maybe Integer
  -> NetlistMonad Component
genComponent' compName componentExpr mStart = do
  LabelM.puts varCount $ fromMaybe 0 mStart
  componentNumber <- getAndModify cmpCount (+1)

  let componentName' = pack
                     . (++ (show componentNumber))
                     . ifThenElse null (++ "Component_") (++ "_")
                     . mkVHDLBasicId
                     $ name2String compName

  (arguments,binders,result) <- splitNormalized componentExpr >>=
                                mkUniqueNormalized

  let ids = HashMap.fromList
          $ map (\(Id v (Embed t)) -> (v,t))
          $ arguments ++ (map fst binders)

  gamma <- (ids `HashMap.union`) . (HashMap.map fst)
           <$> LabelM.gets bindings

  LabelM.puts varEnv gamma

  let resType  = typeToHWType_fail $ ids HashMap.! result
  let argTypes = map (\(Id _ (Embed t)) -> typeToHWType_fail t) arguments

  let netDecls = map (\(id_,_) ->
                        NetDecl (pack . name2String $ varName id_)
                                (typeToHWType_fail . unembed $ varType id_)
                                Nothing
                     ) $ filter ((/= result) . varName . fst) binders
  decls <- concat <$> mapM (uncurry mkConcSm . second unembed) binders

  let compInps       = zip (map (pack . name2String . varName) arguments) argTypes
  let compOutp       = (pack $ name2String result, resType)
  let component      = Component componentName' compInps compOutp (netDecls ++ decls)

  return component

mkConcSm ::
  Id
  -> Term
  -> NetlistMonad [Declaration]
mkConcSm bndr (Var v) = mkApplication bndr v []

mkConcSm bndr app@(App _ _) = do
  let (appF,(args,[])) = second partitionEithers $ collectArgs app
  gamma <- LabelM.gets varEnv
  args' <- Monad.filterM (fmap representableType . termType gamma) args
  case appF of
    Var f
      | all isVar args' -> mkApplication bndr f args'
      | otherwise       -> error "Not in normal form: Var-application with non-Var arguments"
    Data dc
      | all isVar args' -> mkDcApplication bndr dc args'
      | otherwise       -> error "Not in normal form: DataCon-application with non-Var arguments"
    Prim (PrimFun nm _) -> do
      bbM <- fmap (HashMap.lookup . LZ.pack $ name2String nm) $ LabelM.gets primitives
      case bbM of
        Just p@(P.BlackBox {}) -> do
          bbCtx <- mkBlackBoxContext bndr args
          mkBlackBoxDecl p bbCtx
        _ -> error $ "No blackbox found: " ++ show bbM
    _ -> error $ "Not in normal form: application of a Let/Lam/Case" ++ show app

mkConcSm bndr (Core.Literal lit) = do
  let dstId = pack . mkVHDLBasicId . name2String $ varName bndr
  let bndrHWType = typeToHWType_fail . unembed $ varType bndr
  let i = case lit of
            (IntegerLiteral i') -> i'
            _ -> error "not an integer literal"
  return [Assignment dstId Nothing bndrHWType
           [HW.Literal Nothing . NumLit $ fromInteger i]
         ]

mkConcSm _ e = error $ "Not in normal form: let-bound expr is a Let or TyApp: " ++ show e

mkApplication ::
  Id
  -> TmName
  -> [Term]
  -> NetlistMonad [Declaration]
mkApplication dst fun args = do
  normalized <- LabelM.gets bindings
  case HashMap.lookup fun normalized of
    Just _ -> do
      vCnt <- LabelM.gets varCount
      vEnv <- LabelM.gets varEnv
      (Component compName compInps compOutp _) <- genComponent fun Nothing
      LabelM.puts varCount vCnt
      LabelM.puts varEnv vEnv
      case length args == length compInps of
        True  -> do
          let dstId = pack . mkVHDLBasicId . name2String $ varName dst
          let args' = map varToExpr args
          let inpAssigns = zip (map fst compInps) args'
          let outpAssign = (fst compOutp,Identifier dstId Nothing)
          let instDecl = InstDecl compName dstId (outpAssign:inpAssigns)
          return [instDecl]
        False -> error "under-applied normalized function"

    Nothing   -> error ("mkApplication undefined" ++ show (dst,fun,args))

mkDcApplication ::
  Id
  -> DataCon
  -> [Term]
  -> NetlistMonad [Declaration]
mkDcApplication dst dc args = do
  let dstHType = typeToHWType_fail . unembed $ varType dst
  let dstId    = pack . mkVHDLBasicId . name2String $ varName dst
  case dstHType of
    SP _ dcArgPairs -> do
      let dcNameBS = pack . show $ dcName dc
      let dcI      = fromMaybe (error "dc not found") $ elemIndex dcNameBS $ map fst dcArgPairs
      let argTys   = snd $ dcArgPairs !! dcI
      nonEmptyArgs <- Monad.filterM ( return
                                    . not
                                    . isEmptyType
                                    <=< termHWType
                                    ) args
      let nonEmptyArgs' = map varToExpr nonEmptyArgs
      case (compare (length argTys) (length nonEmptyArgs)) of
        EQ -> return [Assignment dstId (Just $ DC dcI) dstHType nonEmptyArgs']
        LT -> error "Over-applied constructor"
        GT -> error "Under-applied constructor"
    _ -> error "mkDcApplication undefined"
