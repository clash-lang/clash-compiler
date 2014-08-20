{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE ViewPatterns     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module CLaSH.GHC.GHC2Core
  ( GHC2CoreState
  , coreToTerm
  , coreToId
  , makeAllTyCons
  , emptyGHC2CoreState
  )
where

-- External Modules
import           Control.Lens                ((^.), (%~), (&), (%=))
import           Control.Monad.State         (State)
import qualified Control.Monad.State.Lazy    as State
import           Data.Hashable               (Hashable (..))
import           Data.HashMap.Lazy           (HashMap)
import qualified Data.HashMap.Lazy           as HashMap
import qualified Data.HashMap.Strict         as HSM
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (isInfixOf,pack)
import qualified Data.Traversable            as T
import           Unbound.LocallyNameless     (Rep, bind, embed, rebind, rec,
                                              runFreshM, string2Name, unbind,
                                              unembed)
import qualified Unbound.LocallyNameless     as Unbound

-- GHC API
import           CLaSH.GHC.Compat.FastString (unpackFB, unpackFS)
import           CLaSH.GHC.Compat.Outputable (showPpr)
import           CLaSH.GHC.Compat.TyCon      (isSuperKindTyCon)
#if __GLASGOW_HASKELL__ < 710
import           Coercion                    (Coercion(..), coercionType)
#else
import           Coercion                    (coercionType)
#endif
import           CoreFVs                     (exprSomeFreeVars)
import           CoreSyn                     (AltCon (..), Bind (..), CoreExpr,
                                              Expr (..), rhssOfAlts)
import           DataCon                     (DataCon, dataConExTyVars,
                                              dataConName, dataConRepArgTys,
                                              dataConTag, dataConTyCon,
                                              dataConUnivTyVars, dataConWorkId,
                                              dataConWrapId_maybe)
import           FamInstEnv                  (FamInst (..), FamInstEnvs,
                                              familyInstances)
import           Id                          (isDataConId_maybe)
import           IdInfo                      (IdDetails (..))
import           Literal                     (Literal (..))
import           Module                      (moduleName, moduleNameString)
import           Name                        (Name, nameModule_maybe,
                                              nameOccName, nameUnique)
import           OccName                     (occNameString)
import           TyCon                       (AlgTyConRhs (..), TyCon,
                                              algTyConRhs, isAlgTyCon,
                                              isFunTyCon, isNewTyCon,
                                              isPrimTyCon,
                                              isSynTyCon, isTupleTyCon,
                                              tcExpandTyCon_maybe, tyConArity,
                                              tyConDataCons, tyConKind,
                                              tyConName, tyConUnique)
import           Type                        (mkTopTvSubst, substTy, tcView)
import           TypeRep                     (TyLit (..), Type (..))
import           Unique                      (Uniquable (..), Unique, getKey)
import           Var                         (Id, TyVar, Var, idDetails,
                                              isTyVar, varName, varType,
                                              varUnique)
import           VarSet                      (isEmptyVarSet)

-- Local imports
import qualified CLaSH.Core.DataCon          as C
import qualified CLaSH.Core.Literal          as C
import qualified CLaSH.Core.Term             as C
import qualified CLaSH.Core.TyCon            as C
import qualified CLaSH.Core.Type             as C
#if __GLASGOW_HASKELL__ < 710
import qualified CLaSH.Core.Util             as C
#endif
import qualified CLaSH.Core.Var              as C
import           CLaSH.Primitives.Types
import           CLaSH.Util

instance Hashable Name where
  hashWithSalt s = hashWithSalt s . getKey . nameUnique

data GHC2CoreState
  = GHC2CoreState
  { _tyConMap :: HashMap C.TyConName TyCon
  , _nameMap  :: HashMap Name String
  }

makeLenses ''GHC2CoreState

emptyGHC2CoreState :: GHC2CoreState
emptyGHC2CoreState = GHC2CoreState HSM.empty HSM.empty

makeAllTyCons :: GHC2CoreState
              -> FamInstEnvs
              -> HashMap C.TyConName C.TyCon
makeAllTyCons hm fiEnvs = go hm hm
  where
    go old new
        | HSM.null (new ^. tyConMap) = HSM.empty
        | otherwise                  = tcm `HSM.union` tcm'
      where
        (tcm,old') = State.runState (T.mapM (makeTyCon fiEnvs) (new ^. tyConMap)) old
        tcm'       = go old' (old' & tyConMap %~ (`HSM.difference` (old ^. tyConMap)))

makeTyCon :: FamInstEnvs
          -> TyCon
          -> State GHC2CoreState C.TyCon
makeTyCon fiEnvs tc = tycon
  where
    tycon
      | isTupleTyCon tc     = mkTupleTyCon
      | isAlgTyCon tc       = mkAlgTyCon
      | isSynTyCon tc       = mkFunTyCon
      | isPrimTyCon tc      = mkPrimTyCon
      | isSuperKindTyCon tc = mkSuperKindTyCon
      | otherwise           = mkVoidTyCon
      where
        tcArity = tyConArity tc

        mkAlgTyCon = do
          tcName <- coreToName tyConName tyConUnique qualfiedNameString tc
          tcKind <- coreToType (tyConKind tc)
          tcRhsM <- makeAlgTyConRhs $ algTyConRhs tc
          case tcRhsM of
            Just tcRhs ->
              return
                C.AlgTyCon
                { C.tyConName   = tcName
                , C.tyConKind   = tcKind
                , C.tyConArity  = tcArity
                , C.algTcRhs    = tcRhs
                }
            Nothing -> return (C.PrimTyCon tcName tcKind tcArity)

        mkFunTyCon = do
          tcName <- coreToName tyConName tyConUnique qualfiedNameString tc
          tcKind <- coreToType (tyConKind tc)
          let instances = familyInstances fiEnvs tc
          substs <- mapM famInstToSubst instances
          return
            C.FunTyCon
            { C.tyConName  = tcName
            , C.tyConKind  = tcKind
            , C.tyConArity = tcArity
            , C.tyConSubst = substs
            }

        mkTupleTyCon = do
          tcName <- coreToName tyConName tyConUnique qualfiedNameString tc
          tcKind <- coreToType (tyConKind tc)
          tcDc   <- fmap (C.DataTyCon . (:[])) . coreToDataCon False . head . tyConDataCons $ tc
          return
            C.AlgTyCon
            { C.tyConName   = tcName
            , C.tyConKind   = tcKind
            , C.tyConArity  = tcArity
            , C.algTcRhs    = tcDc
            }

        mkPrimTyCon = do
          tcName <- coreToName tyConName tyConUnique qualfiedNameString tc
          tcKind <- coreToType (tyConKind tc)
          return
            C.PrimTyCon
            { C.tyConName    = tcName
            , C.tyConKind    = tcKind
            , C.tyConArity   = tcArity
            }

        mkSuperKindTyCon = do
          tcName <- coreToName tyConName tyConUnique qualfiedNameString tc
          return C.SuperKindTyCon
                   { C.tyConName = tcName
                   }

        mkVoidTyCon = do
          tcName <- coreToName tyConName tyConUnique qualfiedNameString tc
          tcKind <- coreToType (tyConKind tc)
          return (C.PrimTyCon tcName tcKind tcArity)

        famInstToSubst :: FamInst -> State GHC2CoreState ([C.Type],C.Type)
        famInstToSubst fi = do
          tys <- mapM coreToType  (fi_tys fi)
          ty  <- coreToType (fi_rhs fi)
          return (tys,ty)

makeAlgTyConRhs :: AlgTyConRhs
                -> State GHC2CoreState (Maybe C.AlgTyConRhs)
makeAlgTyConRhs algTcRhs = case algTcRhs of
  DataTyCon dcs _ -> Just <$> C.DataTyCon <$> mapM (coreToDataCon False) dcs
  NewTyCon dc _ (rhsTvs,rhsEtad) _ -> Just <$> (C.NewTyCon <$> coreToDataCon False dc
                                                           <*> ((,) <$> mapM coreToVar rhsTvs
                                                                    <*> coreToType rhsEtad
                                                               )
                                               )
  AbstractTyCon _ -> return Nothing
  DataFamilyTyCon -> return Nothing

coreToTerm :: PrimMap
           -> [Var]
           -> CoreExpr
           -> State GHC2CoreState C.Term
coreToTerm primMap unlocs coreExpr = term coreExpr
  where
    term (Var x)                 = var x
    term (Lit l)                 = return $ C.Literal (coreToLiteral l)
    term (App eFun (Type tyArg)) = C.TyApp <$> term eFun <*> coreToType tyArg
    term (App eFun eArg)         = C.App   <$> term eFun <*> term eArg
    term (Lam x e) | isTyVar x   = C.TyLam <$> (bind <$> coreToTyVar x <*> term e)
                   | otherwise   = C.Lam   <$> (bind <$> coreToId x    <*> term e)
    term (Let (NonRec x e1) e2)  = do
      x' <- coreToId x
      e1' <- term e1
      e2' <- term e2
      return $ C.Letrec $ bind (rec [(x', embed e1')]) e2'

    term (Let (Rec xes) e) = do
      xes' <- mapM
                ( firstM  coreToId <=<
                  secondM ((return . embed) <=< term)
                ) xes
      e' <- term e
      return $ C.Letrec $ bind (rec xes') e'

    term (Case _ _ ty [])  = C.Prim (pack "EmptyCase") <$> coreToType ty
    term (Case e b ty alts) = do
     let usesBndr = any ( not . isEmptyVarSet . exprSomeFreeVars (`elem` [b]))
                  $ rhssOfAlts alts
     b' <- coreToId b
     e' <- term e
     ty' <- coreToType ty
     let caseTerm v = C.Case v ty' <$> mapM alt alts
     if usesBndr
      then do
        ct <- caseTerm (C.Var (unembed $ C.varType b') (C.varName b'))
        return $ C.Letrec $ bind (rec [(b',embed e')]) ct
      else caseTerm e'

#if __GLASGOW_HASKELL__ < 710
    term (Cast e co)       = do
      e' <- term e
      case C.collectArgs e' of
        (C.Prim nm pTy, [Right _, Left errMsg])
          | nm == (pack "Control.Exception.Base.irrefutPatError") -> case co of
            (UnivCo _ _ resTy) -> do resTy' <- coreToType resTy
                                     return (C.mkApps (C.Prim nm pTy) [Right resTy', Left errMsg])
            _ -> error $ $(curLoc) ++ "irrefutPatError casted with an unknown coercion: " ++ showPpr co
        _ -> return e'
#else
    term (Cast e _)        = term e
#endif

    term (Tick _ e)        = term e
    term (Type t)          = C.Prim (pack "_TY_") <$> coreToType t
    term (Coercion co)     = C.Prim (pack "_CO_") <$> coreToType (coercionType co)

    var x = do
        xVar   <- coreToVar x
        xPrim  <- coreToPrimVar x
        let xNameS = pack $ Unbound.name2String xPrim
        xType  <- coreToType (varType x)
        case isDataConId_maybe x of
          Just dc -> case HashMap.lookup xNameS primMap of
                      Just _  -> return $ C.Prim xNameS xType
                      Nothing -> C.Data <$> coreToDataCon (isDataConWrapId x && not (isNewTyCon (dataConTyCon dc))) dc
          Nothing -> case HashMap.lookup xNameS primMap of
            Just (Primitive f _)
              | f == pack "CLaSH.Signal.Internal.mapSignal#" -> return (mapSignalTerm xType)
              | f == pack "CLaSH.Signal.Internal.signal#"    -> return (signalTerm xType)
              | f == pack "CLaSH.Signal.Internal.appSignal#" -> return (appSignalTerm xType)
              | f == pack "CLaSH.Signal.Wrap.vecUnwrap#"     -> return (vecUnwrapTerm xType)
              | f == pack "CLaSH.Signal.Wrap.vecWrap#"       -> return (vecWrapTerm xType)
              | f == pack "GHC.Base.$"                       -> return (dollarTerm xType)
              | otherwise                                    -> return (C.Prim xNameS xType)
            Just (BlackBox {}) ->
              return (C.Prim xNameS xType)
            Nothing
              | x `elem` unlocs -> return (C.Prim xNameS xType)
              | pack "$cshow" `isInfixOf` xNameS -> return (C.Prim xNameS xType)
              | otherwise       -> return  (C.Var xType xVar)

    alt (DEFAULT   , _ , e) = bind C.DefaultPat <$> term e
    alt (LitAlt l  , _ , e) = bind (C.LitPat . embed $ coreToLiteral l) <$> term e
    alt (DataAlt dc, xs, e) = case span isTyVar xs of
      (tyvs,tmvs) -> bind <$> (C.DataPat . embed <$>
                                coreToDataCon False dc <*>
                                (rebind <$>
                                  mapM coreToTyVar tyvs <*>
                                  mapM coreToId tmvs)) <*>
                              term e

    coreToLiteral :: Literal
                  -> C.Literal
    coreToLiteral l = case l of
      MachStr    fs  -> C.StringLiteral (unpackFB fs)
      MachChar   c   -> C.StringLiteral [c]
      MachInt    i   -> C.IntegerLiteral i
      MachInt64  i   -> C.IntegerLiteral i
      MachWord   i   -> C.IntegerLiteral i
      MachWord64 i   -> C.IntegerLiteral i
      LitInteger i _ -> C.IntegerLiteral i
      MachFloat r    -> C.RationalLiteral r
      MachDouble r   -> C.RationalLiteral r
      MachNullAddr   -> C.StringLiteral []
      _              -> error $ $(curLoc) ++ "Can't convert literal: " ++ showPpr l ++ " in expression: " ++ showPpr coreExpr

coreToDataCon :: Bool
              -> DataCon
              -> State GHC2CoreState C.DataCon
coreToDataCon mkWrap dc = do
    repTys <- mapM coreToType (dataConRepArgTys dc)
    dcTy   <- if mkWrap
                then case dataConWrapId_maybe dc of
                        Just wrapId -> coreToType (varType wrapId)
                        Nothing     -> error $ $(curLoc) ++ "DataCon Wrapper: " ++ showPpr dc ++ " not found"
                else coreToType (varType $ dataConWorkId dc)
    mkDc dcTy repTys
  where
    mkDc dcTy repTys = do
      nm   <- coreToName dataConName getUnique qualfiedNameString dc
      uTvs <- mapM coreToVar (dataConUnivTyVars dc)
      eTvs <- mapM coreToVar (dataConExTyVars dc)
      return $ C.MkData
             { C.dcName       = nm
             , C.dcTag        = dataConTag dc
             , C.dcType       = dcTy
             , C.dcArgTys     = repTys
             , C.dcUnivTyVars = uTvs
             , C.dcExtTyVars  = eTvs
             }

coreToType :: Type
           -> State GHC2CoreState C.Type
coreToType ty = coreToType' $ fromMaybe ty (tcView ty)

coreToType' :: Type
            -> State GHC2CoreState C.Type
coreToType' (TyVarTy tv) = C.VarTy <$> coreToType (varType tv) <*> (coreToVar tv)
coreToType' (TyConApp tc args)
  | isFunTyCon tc = foldl C.AppTy (C.ConstTy C.Arrow) <$> mapM coreToType args
  | otherwise     = case tcExpandTyCon_maybe tc args of
                      Just (substs,synTy,remArgs) -> do
                        let substs' = mkTopTvSubst substs
                            synTy'  = substTy substs' synTy
                        foldl C.AppTy <$> coreToType synTy' <*> mapM coreToType remArgs
                      _ -> do
                        tcName <- coreToName tyConName tyConUnique qualfiedNameString tc
                        tyConMap %= (HSM.insert tcName tc)
                        C.mkTyConApp <$> (pure tcName) <*> mapM coreToType args
coreToType' (FunTy ty1 ty2)  = C.mkFunTy <$> coreToType ty1 <*> coreToType ty2
coreToType' (ForAllTy tv ty) = C.ForAllTy <$>
                               (bind <$> coreToTyVar tv <*> coreToType ty)
coreToType' (LitTy tyLit)    = return $ C.LitTy (coreToTyLit tyLit)
coreToType' (AppTy ty1 ty2)  = C.AppTy <$> coreToType ty1 <*> coreToType' ty2

coreToTyLit :: TyLit
            -> C.LitTy
coreToTyLit (NumTyLit i) = C.NumTy (fromInteger i)
coreToTyLit (StrTyLit s) = C.SymTy (unpackFS s)

coreToTyVar :: TyVar
            -> State GHC2CoreState C.TyVar
coreToTyVar tv =
  C.TyVar <$> (coreToVar tv) <*> (embed <$> coreToType (varType tv))

coreToId :: Id
         -> State GHC2CoreState C.Id
coreToId i =
  C.Id <$> (coreToVar i) <*> (embed <$> coreToType (varType i))

coreToVar :: Rep a
          => Var
          -> State GHC2CoreState (Unbound.Name a)
coreToVar = coreToName varName varUnique qualfiedNameStringM

coreToPrimVar :: Var
              -> State GHC2CoreState (Unbound.Name C.Term)
coreToPrimVar = coreToName varName varUnique qualfiedNameString

coreToName :: Rep a
           => (b -> Name)
           -> (b -> Unique)
           -> (Name -> State GHC2CoreState String)
           -> b
           -> State GHC2CoreState (Unbound.Name a)
coreToName toName toUnique toString v = do
  ns <- toString (toName v)
  return (Unbound.makeName ns (toInteger . getKey . toUnique $ v))

qualfiedNameString :: Name
                   -> State GHC2CoreState String
qualfiedNameString n = makeCached n nameMap
                     $ return (fromMaybe "_INTERNAL_" (modNameM n) ++ ('.':occName))
  where
    occName = occNameString $ nameOccName n

qualfiedNameStringM :: Name
                    -> State GHC2CoreState String
qualfiedNameStringM n = makeCached n nameMap
                      $ return (maybe occName (\modName -> modName ++ ('.':occName)) (modNameM n))
  where
    occName = occNameString $ nameOccName n

modNameM :: Name
         -> Maybe String
modNameM n = do
      module_ <- nameModule_maybe n
      let moduleNm = moduleName module_
      return (moduleNameString moduleNm)

-- | Given the type:
--
-- @forall a. forall b. forall clk. (a -> b) -> CSignal clk a -> CSignal clk b@
--
-- Generate the term:
--
-- @
-- /\(a:*)./\(b:*)./\(clk:Clock).\(f : (CSignal clk a -> CSignal clk b)).
-- \(x : CSignal clk a).f x
-- @
mapSignalTerm :: C.Type
              -> C.Term
mapSignalTerm (C.ForAllTy tvATy) =
    C.TyLam (bind aTV (
    C.TyLam (bind bTV (
    C.TyLam (bind clkTV (
    C.Lam   (bind fId (
    C.Lam   (bind xId (
    C.App (C.Var fTy fName) (C.Var aTy xName)))))))))))
  where
    (aTV,bTV,clkTV,funTy) = runFreshM $ do
      { (aTV',C.ForAllTy tvBTy)   <- unbind tvATy
      ; (bTV',C.ForAllTy tvClkTy) <- unbind tvBTy
      ; (clkTV',funTy')           <- unbind tvClkTy
      ; return (aTV',bTV',clkTV',funTy')
      }
    (C.FunTy _ funTy'') = C.tyView funTy
    (C.FunTy aTy bTy)   = C.tyView funTy''
    fName = string2Name "f"
    xName = string2Name "x"
    fTy   = C.mkFunTy aTy bTy
    fId   = C.Id fName (embed fTy)
    xId   = C.Id xName (embed aTy)

mapSignalTerm ty = error $ $(curLoc) ++ show ty

-- | Given the type:
--
-- @forall a. forall clk. a -> CSignal clk a@
--
-- Generate the term
--
-- @/\(a:*)./\(clk:Clock).\(x:CSignal clk a).x@
signalTerm :: C.Type
           -> C.Term
signalTerm (C.ForAllTy tvATy) =
    C.TyLam (bind aTV (
    C.TyLam (bind clkTV (
    C.Lam   (bind xId (
    C.Var   aTy xName))))))
  where
    (aTV,clkTV,funTy) = runFreshM $ do
      { (aTV', C.ForAllTy tvClkTy) <- unbind tvATy
      ; (clkTV', funTy')           <- unbind tvClkTy
      ; return (aTV',clkTV',funTy')
      }
    (C.FunTy _ aTy) = C.tyView funTy
    xName = string2Name "x"
    xId   = C.Id xName (embed aTy)

signalTerm ty = error $ $(curLoc) ++ show ty

-- | Given the type:
--
-- @
-- forall clk. forall a. forall b. CSignal clk (a -> b) -> CSignal clk a ->
-- CSignal clk b
-- @
--
-- Generate the term:
--
-- @
-- /\(clk:Clock)./\(a:*)./\(b:*).\(f : (CSignal clk a -> CSignal clk b)).
-- \(x : CSignal clk a).f x
-- @
appSignalTerm :: C.Type
              -> C.Term
appSignalTerm (C.ForAllTy tvClkTy) =
    C.TyLam (bind clkTV (
    C.TyLam (bind aTV (
    C.TyLam (bind bTV (
    C.Lam   (bind fId (
    C.Lam   (bind xId (
    C.App (C.Var fTy fName) (C.Var aTy xName)))))))))))
  where
    (clkTV,aTV,bTV,funTy) = runFreshM $ do
      { (clkTV',C.ForAllTy tvATy) <- unbind tvClkTy
      ; (aTV',C.ForAllTy tvBTy)   <- unbind tvATy
      ; (bTV',funTy')           <- unbind tvBTy
      ; return (clkTV',aTV',bTV',funTy')
      }
    (C.FunTy _ funTy'') = C.tyView funTy
    (C.FunTy aTy bTy)   = C.tyView funTy''
    fName = string2Name "f"
    xName = string2Name "x"
    fTy   = C.mkFunTy aTy bTy
    fId   = C.Id fName (embed fTy)
    xId   = C.Id xName (embed aTy)

appSignalTerm ty = error $ $(curLoc) ++ show ty

-- | Given the type:
--
-- @
-- forall t.forall n.forall a.SClock t -> Vec n (CSignal t a) ->
-- CSignal t (Vec n a)
-- @
--
-- Generate the term:
--
-- @/\(t:Clock)./\(n:Nat)./\(a:*).\(sclk:SClock t).\(vs:CSignal t (Vec n a)).vs@
vecUnwrapTerm :: C.Type
              -> C.Term
vecUnwrapTerm (C.ForAllTy tvTTy) =
    C.TyLam (bind tTV (
    C.TyLam (bind nTV (
    C.TyLam (bind aTV (
    C.Lam   (bind sclkId (
    C.Lam   (bind vsId (
    C.Var vsTy vsName))))))))))
  where
    (tTV,nTV,aTV,funTy) = runFreshM $ do
      { (tTV',C.ForAllTy tvNTy) <- unbind tvTTy
      ; (nTV',C.ForAllTy tvATy) <- unbind tvNTy
      ; (aTV',funTy')           <- unbind tvATy
      ; return (tTV',nTV',aTV',funTy')
      }
    (C.FunTy sclkTy funTy'') = C.tyView funTy
    (C.FunTy _ vsTy)         = C.tyView funTy''
    sclkName = string2Name "sclk"
    vsName   = string2Name "vs"
    sclkId   = C.Id sclkName (embed sclkTy)
    vsId     = C.Id vsName   (embed vsTy)

vecUnwrapTerm ty = error $ $(curLoc) ++ show ty

-- | Given the type:
--
-- @
-- forall n.forall t.forall a.KnownNat n => SClock t -> CSignal (Vec n a) ->
-- Vec n (CSignal t a)
-- @
--
-- Generate the term:
--
-- @
-- /\(n:Nat)./\(t:Clock)./\(a:*).\(dict:KnownNat n).\(sclk:SClock t).
-- \(vs:CSignal (Vec n a)).vs
-- @
vecWrapTerm :: C.Type
              -> C.Term
vecWrapTerm (C.ForAllTy tvNTy) =
    C.TyLam (bind nTV (
    C.TyLam (bind tTV (
    C.TyLam (bind aTV (
    C.Lam   (bind dictId (
    C.Lam   (bind sclkId (
    C.Lam   (bind vsId (
    C.Var vsTy vsName))))))))))))
  where
    (nTV,tTV,aTV,funTy) = runFreshM $ do
      { (nTV',C.ForAllTy tvTTy) <- unbind tvNTy
      ; (tTV',C.ForAllTy tvATy) <- unbind tvTTy
      ; (aTV',funTy')           <- unbind tvATy
      ; return (nTV',tTV',aTV',funTy')
      }
    (C.FunTy dictTy funTy'') = C.tyView funTy
    (C.FunTy sclkTy funTy3)  = C.tyView funTy''
    (C.FunTy vsTy _)         = C.tyView funTy3
    dictName = string2Name "dict"
    sclkName = string2Name "sclk"
    vsName   = string2Name "vs"
    dictId   = C.Id dictName (embed dictTy)
    sclkId   = C.Id sclkName (embed sclkTy)
    vsId     = C.Id vsName   (embed vsTy)

vecWrapTerm ty = error $ $(curLoc) ++ show ty

-- | Given the type:
--
-- @forall a. forall b. (a -> b) -> a -> b@
--
-- Generate the term:
--
-- @/\(a:*)./\(b:*).\(f : (a -> b)).\(x : a).f x@
dollarTerm :: C.Type
           -> C.Term
dollarTerm (C.ForAllTy tvATy) =
    C.TyLam (bind aTV (
    C.TyLam (bind bTV (
    C.Lam   (bind fId (
    C.Lam   (bind xId (
    C.App (C.Var fTy fName) (C.Var aTy xName)))))))))
  where
    (aTV,bTV,funTy) = runFreshM $ do
      { (aTV',C.ForAllTy tvBTy) <- unbind tvATy
      ; (bTV',funTy')           <- unbind tvBTy
      ; return (aTV',bTV',funTy')
      }
    (C.FunTy fTy funTy'') = C.tyView funTy
    (C.FunTy aTy _)       = C.tyView funTy''
    fName = string2Name "f"
    xName = string2Name "x"
    fId   = C.Id fName (embed fTy)
    xId   = C.Id xName (embed aTy)

dollarTerm ty = error $ $(curLoc) ++ show ty

isDataConWrapId :: Id -> Bool
isDataConWrapId v = case idDetails v of
                      DataConWrapId {} -> True
                      _                -> False
