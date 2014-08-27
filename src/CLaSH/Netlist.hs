{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

-- | Create Netlists out of normalized CoreHW Terms
module CLaSH.Netlist where

import           Control.Lens               ((.=), (<<%=))
import qualified Control.Lens               as Lens
import           Control.Monad.State        (runStateT)
import           Control.Monad.Writer       (listen, runWriterT)
import           Data.Either                (lefts,partitionEithers)
import           Data.HashMap.Lazy          (HashMap)
import qualified Data.HashMap.Lazy          as HashMap
import qualified Data.HashSet               as HashSet
import           Data.List                  (elemIndex, nub)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text.Lazy             as Text
import           Unbound.LocallyNameless    (Embed (..), name2String,
                                             runFreshMT, unbind, unembed,
                                             unrebind)

import           CLaSH.Core.DataCon         (DataCon (..))
import           CLaSH.Core.Literal         (Literal (..))
import           CLaSH.Core.Pretty          (showDoc)
import           CLaSH.Core.Term            (Pat (..), Term (..), TmName)
import qualified CLaSH.Core.Term            as Core
import           CLaSH.Core.Type            (Type (..))
import           CLaSH.Core.TyCon           (TyConName, TyCon)
import           CLaSH.Core.Util            (collectArgs, isVar, termType)
import           CLaSH.Core.Var             (Id, Var (..))
import           CLaSH.Netlist.BlackBox
import           CLaSH.Netlist.Id
import           CLaSH.Netlist.Types        as HW
import           CLaSH.Netlist.Util
import           CLaSH.Normalize.Util
import           CLaSH.Primitives.Types     as P
import           CLaSH.Util

-- | Generate a hierarchical netlist out of a set of global binders with
-- @topEntity@ at the top.
genNetlist :: Maybe VHDLState
           -- ^ State for the 'CLaSH.Netlist.VHDL.VHDLM' Monad
           -> Maybe Int
           -- ^ Starting number of the component counter
           -> HashMap TmName (Type,Term)
           -- ^ Global binders
           -> PrimMap
           -- ^ Primitive definitions
           -> HashMap TyConName TyCon
           -- ^ TyCon cache
           -> (HashMap TyConName TyCon -> Type -> Maybe (Either String HWType))
           -- ^ Hardcoded Type -> HWType translator
           -> Maybe Int
           -- ^ Symbol count
           -> TmName
           -- ^ Name of the @topEntity@
           -> IO ([Component],VHDLState,Int)
genNetlist vhdlStateM compCntM globals primMap tcm typeTrans mStart topEntity = do
  (_,s) <- runNetlistMonad vhdlStateM compCntM globals primMap tcm typeTrans $ genComponent topEntity mStart
  return (HashMap.elems $ _components s, _vhdlMState s, _cmpCount s)

-- | Run a NetlistMonad action in a given environment
runNetlistMonad :: Maybe VHDLState
                -- ^ State for the 'CLaSH.Netlist.VHDL.VHDLM' Monad
                -> Maybe Int
                -- ^ Starting number of the component counter
                -> HashMap TmName (Type,Term)
                -- ^ Global binders
                -> PrimMap
                -- ^ Primitive Definitions
                -> HashMap TyConName TyCon
                -- ^ TyCon cache
                -> (HashMap TyConName TyCon -> Type -> Maybe (Either String HWType))
                -- ^ Hardcode Type -> HWType translator
                -> NetlistMonad a
                -- ^ Action to run
                -> IO (a,NetlistState)
runNetlistMonad vhdlStateM compCntM s p tcm typeTrans
  = runFreshMT
  . flip runStateT s'
  . (fmap fst . runWriterT)
  . runNetlist
  where
    s' = NetlistState s HashMap.empty 0 (fromMaybe 0 compCntM) HashMap.empty p (fromMaybe (HashSet.empty,0,HashMap.empty) vhdlStateM) typeTrans tcm

-- | Generate a component for a given function (caching)
genComponent :: TmName -- ^ Name of the function
             -> Maybe Int -- ^ Starting value of the unique counter
             -> NetlistMonad Component
genComponent compName mStart = do
  compExprM <- fmap (HashMap.lookup compName) $ Lens.use bindings
  case compExprM of
    Nothing -> error $ $(curLoc) ++ "No normalized expression found for: " ++ show compName
    Just (_,expr) -> makeCached compName components $
                      genComponentT compName expr mStart

-- | Generate a component for a given function
genComponentT :: TmName -- ^ Name of the function
              -> Term -- ^ Corresponding term
              -> Maybe Int -- ^ Starting value of the unique counter
              -> NetlistMonad Component
genComponentT compName componentExpr mStart = do
  varCount .= fromMaybe 0 mStart
  componentNumber <- cmpCount <<%= (+1)

  let componentName' = (`Text.append` (Text.pack $ show componentNumber))
                     . ifThenElse Text.null
                          (`Text.append` Text.pack "Component_")
                          (`Text.append` Text.pack "_")
                     . mkBasicId' True
                     . stripDollarPrefixes
                     . last
                     . Text.splitOn (Text.pack ".")
                     . Text.pack
                     $ name2String compName

  tcm <- Lens.use tcCache
  (arguments,binders,result) <- do { normalizedM <- splitNormalized tcm componentExpr
                                   ; case normalizedM of
                                       Right normalized -> mkUniqueNormalized normalized
                                       Left err         -> error err
                                   }

  let ids = HashMap.fromList
          $ map (\(Id v (Embed t)) -> (v,t))
          $ arguments ++ map fst binders

  gamma <- (ids `HashMap.union`) . HashMap.map fst
           <$> Lens.use bindings

  varEnv .= gamma

  typeTrans    <- Lens.use typeTranslator
  let resType  = unsafeCoreTypeToHWType $(curLoc) typeTrans tcm $ HashMap.lookupDefault (error $ $(curLoc) ++ "resType" ++ show (result,HashMap.keys ids)) result ids
      argTypes = map (\(Id _ (Embed t)) -> unsafeCoreTypeToHWType $(curLoc) typeTrans tcm t) arguments

  let netDecls = map (\(id_,_) ->
                        NetDecl (mkBasicId . Text.pack . name2String $ varName id_)
                                (unsafeCoreTypeToHWType $(curLoc) typeTrans tcm . unembed $ varType id_)
                                Nothing
                     ) $ filter ((/= result) . varName . fst) binders
  (decls,clks) <- listen $ concat <$> mapM (uncurry mkDeclarations . second unembed) binders

  let compInps       = zip (map (mkBasicId . Text.pack . name2String . varName) arguments) argTypes
      compOutp       = (mkBasicId . Text.pack $ name2String result, resType)
      component      = Component componentName' (nub clks) compInps compOutp (netDecls ++ decls)
  return component

-- | Generate a list of Declarations for a let-binder
mkDeclarations :: Id -- ^ LHS of the let-binder
               -> Term -- ^ RHS of the let-binder
               -> NetlistMonad [Declaration]
mkDeclarations bndr (Var _ v) = mkFunApp bndr v []

mkDeclarations _ e@(Case _ _ []) =
  error $ $(curLoc) ++ "Not in normal form: Case-decompositions with an empty list of alternatives not supported: " ++ showDoc e

mkDeclarations bndr e@(Case (Var scrutTy scrutNm) _ [alt]) = do
  (pat,v) <- unbind alt
  (varTy,varTm) <- case v of
                     (Var t n) -> return (t,n)
                     _ -> error $ $(curLoc) ++ "Not in normal form: RHS of case-projection is not a variable: " ++ showDoc e
  typeTrans    <- Lens.use typeTranslator
  tcm          <- Lens.use tcCache
  let dstId    = mkBasicId . Text.pack . name2String $ varName bndr
      altVarId = mkBasicId . Text.pack $ name2String varTm
      selId    = mkBasicId . Text.pack $ name2String scrutNm
      modifier = case pat of
        DataPat (Embed dc) ids -> let (_,tms) = unrebind ids
                                  in case elemIndex (Id varTm (Embed varTy)) tms of
                                       Nothing -> Nothing
                                       Just fI -> Just (Indexed (unsafeCoreTypeToHWType $(curLoc) typeTrans tcm scrutTy,dcTag dc - 1,fI))
        _                      -> error $ $(curLoc) ++ "Not in normal form: Unexpected pattern in case-projection: " ++ showDoc e
      extractExpr = Identifier (maybe altVarId (const selId) modifier) modifier
  return [Assignment dstId extractExpr]

mkDeclarations bndr (Case scrut altTy alts) = do
  alts'                  <- mapM unbind alts
  tcm                    <- Lens.use tcCache
  scrutTy                <- termType tcm scrut
  scrutHTy               <- unsafeCoreTypeToHWTypeM $(curLoc) scrutTy
  (scrutExpr,scrutDecls) <- first (mkScrutExpr scrutHTy (fst (last alts'))) <$> mkExpr scrutTy scrut
  (exprs,altsDecls)      <- (second concat . unzip) <$> mapM (mkCondExpr scrutHTy) alts'

  let dstId = mkBasicId . Text.pack . name2String $ varName bndr
  return $! scrutDecls ++ altsDecls ++ [CondAssignment dstId scrutExpr (reverse exprs)]
  where
    mkCondExpr :: HWType -> (Pat,Term) -> NetlistMonad ((Maybe Expr,Expr),[Declaration])
    mkCondExpr scrutHTy (pat,alt) = do
      (altExpr,altDecls) <- mkExpr altTy alt
      (,altDecls) <$> case pat of
        DefaultPat           -> return (Nothing,altExpr)
        DataPat (Embed dc) _ -> return (Just (dcToLiteral scrutHTy (dcTag dc)),altExpr)
        LitPat  (Embed (IntegerLiteral i)) -> return (Just (HW.Literal Nothing (NumLit $ fromInteger i)),altExpr)
        _                    -> error $ $(curLoc) ++ "Not an integer literal in LitPat"

    mkScrutExpr :: HWType -> Pat -> Expr -> Expr
    mkScrutExpr scrutHTy pat scrutE = case pat of
      DataPat (Embed dc) _ -> let modifier = Just (DC (scrutHTy,dcTag dc - 1))
                              in case scrutE of
                                  Identifier scrutId _ -> Identifier scrutId modifier
                                  BlackBoxE bbE _      -> BlackBoxE bbE modifier
                                  _ -> error $ $(curLoc) ++ "Not in normal form: Not a variable reference or primitive as subject of a case-statement"
      _ -> scrutE

mkDeclarations bndr app =
  let (appF,(args,tyArgs)) = second partitionEithers $ collectArgs app
  in case appF of
    Var _ f
      | null tyArgs -> mkFunApp bndr f args
      | otherwise   -> error $ $(curLoc) ++ "Not in normal form: Var-application with Type arguments"
    _ -> do
      (exprApp,declsApp) <- mkExpr (unembed $ varType bndr) app
      let dstId = mkBasicId . Text.pack . name2String $ varName bndr
      return (declsApp ++ [Assignment dstId exprApp])

-- | Generate a list of Declarations for a let-binder where the RHS is a function application
mkFunApp :: Id -- ^ LHS of the let-binder
         -> TmName -- ^ Name of the applied function
         -> [Term] -- ^ Function arguments
         -> NetlistMonad [Declaration]
mkFunApp dst fun args = do
  normalized <- Lens.use bindings
  case HashMap.lookup fun normalized of
    Just _ -> do
      (Component compName hidden compInps compOutp _) <- preserveVarEnv $ genComponent fun Nothing
      if length args == length compInps
        then do tcm <- Lens.use tcCache
                argTys              <- mapM (termType tcm) args
                (argExprs,argDecls) <- fmap (second concat . unzip) $! mapM (\(e,t) -> mkExpr t e) (zip args argTys)
                let dstId         = mkBasicId . Text.pack . name2String $ varName dst
                    hiddenAssigns = map (\(i,_) -> (i,Identifier i Nothing)) hidden
                    inpAssigns    = zip (map fst compInps) argExprs
                    outpAssign    = (fst compOutp,Identifier dstId Nothing)
                    instLabel     = Text.concat [compName, Text.pack "_", dstId]
                    instDecl      = InstDecl compName instLabel (outpAssign:hiddenAssigns ++ inpAssigns)
                return (argDecls ++ [instDecl])
        else error $ $(curLoc) ++ "under-applied normalized function"
    Nothing -> case args of
      [] -> do
        let dstId = mkBasicId . Text.pack . name2String $ varName dst
        return [Assignment dstId (Identifier (mkBasicId . Text.pack $ name2String fun) Nothing)]
      _ -> error $ $(curLoc) ++ "Unknown function: " ++ showDoc fun

-- | Generate an expression for a term occurring on the RHS of a let-binder
mkExpr :: Type -- ^ Type of the LHS of the let-binder
       -> Term -- ^ Term to convert to an expression
       -> NetlistMonad (Expr,[Declaration]) -- ^ Returned expression and a list of generate BlackBox declarations
mkExpr _ (Core.Literal lit) = return (HW.Literal Nothing . NumLit $ fromInteger  $! i,[])
  where
    i = case lit of
          (IntegerLiteral i') -> i'
          _ -> error $ $(curLoc) ++ "not an integer literal"

mkExpr ty app = do
  let (appF,args) = collectArgs app
      tmArgs      = lefts args
  hwTy    <- unsafeCoreTypeToHWTypeM $(curLoc) ty
  case appF of
    Data dc
      | all (\e -> isConstant e || isVar e) tmArgs -> mkDcApplication hwTy dc tmArgs
      | otherwise                                  -> error $ $(curLoc) ++ "Not in normal form: DataCon-application with non-Simple arguments"
    Prim nm _ -> first fst <$> mkPrimitive False nm args ty
    Var _ f
      | null tmArgs -> return (Identifier (mkBasicId . Text.pack $ name2String f) Nothing,[])
      | otherwise -> error $ $(curLoc) ++ "Not in normal form: top-level binder in argument position: " ++ showDoc app
    _ -> error $ $(curLoc) ++ "Not in normal form: application of a Let/Lam/Case: " ++ showDoc app

-- | Generate an expression for a DataCon application occurring on the RHS of a let-binder
mkDcApplication :: HWType -- ^ HWType of the LHS of the let-binder
                -> DataCon -- ^ Applied DataCon
                -> [Term] -- ^ DataCon Arguments
                -> NetlistMonad (Expr,[Declaration]) -- ^ Returned expression and a list of generate BlackBox declarations
mkDcApplication dstHType dc args = do
  tcm                 <- Lens.use tcCache
  argTys              <- mapM (termType tcm) args
  (argExprs,argDecls) <- fmap (second concat . unzip) $! mapM (\(e,t) -> mkExpr t e) (zip args argTys)
  argHWTys            <- mapM coreTypeToHWTypeM argTys
  fmap (,argDecls) $! case (argHWTys,argExprs) of
    -- Is the DC just a newtype wrapper?
    ([Just argHwTy],[argExpr]) | argHwTy == dstHType -> return argExpr
    _ -> case dstHType of
      SP _ dcArgPairs -> do
        let dcI      = dcTag dc - 1
            dcArgs   = snd $ indexNote ($(curLoc) ++ "No DC with tag: " ++ show dcI) dcArgPairs dcI
        case compare (length dcArgs) (length argExprs) of
          EQ -> return (HW.DataCon dstHType (Just $ DC (dstHType,dcI)) argExprs)
          LT -> error $ $(curLoc) ++ "Over-applied constructor"
          GT -> error $ $(curLoc) ++ "Under-applied constructor"
      Product _ dcArgs ->
        case compare (length dcArgs) (length argExprs) of
          EQ -> return (HW.DataCon dstHType (Just $ DC (dstHType,0)) argExprs)
          LT -> error $ $(curLoc) ++ "Over-applied constructor"
          GT -> error $ $(curLoc) ++ "Under-applied constructor"
      Sum _ _ ->
        return (HW.DataCon dstHType (Just $ DC (dstHType,dcTag dc - 1)) [])
      Bool ->
        let dc' = case dcTag dc of
                   1  -> HW.Literal Nothing (BoolLit False)
                   2  -> HW.Literal Nothing (BoolLit True)
                   tg -> error $ $(curLoc) ++ "unknown bool literal: " ++ showDoc dc ++ "(tag: " ++ show tg ++ ")"
        in  return dc'
      Vector 0 _ -> return (HW.DataCon dstHType Nothing [])
      -- Note [Vector Wrapper]
      -- The Vector type has two versions of the cons constructor:
      --   * The 'normal' one, which takes a coercion as its first argument,
      --     followed by the element and the vector
      --   * The wrapper one, which just takes the element and vector argument
      --
      -- We need to account for both occurrences, that's why we have the two
      -- case statements below:
      Vector 1 _ -> case argExprs of
                      [_,e,_] -> return (HW.DataCon dstHType (Just VecAppend) [e])
                      _       -> return (HW.DataCon dstHType (Just VecAppend) [head argExprs])
      Vector _ _ -> case argExprs of
                      [_,e1,e2] -> return (HW.DataCon dstHType (Just VecAppend) [e1,e2])
                      _         -> return (HW.DataCon dstHType (Just VecAppend) argExprs)

      _ -> error $ $(curLoc) ++ "mkDcApplication undefined for: " ++ show (dstHType,dc,args,argHWTys)
