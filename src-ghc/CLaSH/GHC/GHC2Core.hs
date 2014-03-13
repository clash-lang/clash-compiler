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
import           Data.Text                   (pack)
import qualified Data.Traversable            as T
import           Unbound.LocallyNameless     (Rep, bind, embed, rebind, rec,
                                              runFreshM, string2Name, unbind,
                                              unembed)
import qualified Unbound.LocallyNameless     as Unbound

-- GHC API
import           CLaSH.GHC.Compat.FastString (unpackFB, unpackFS)
import           CLaSH.GHC.Compat.Outputable (showPpr)
import           CLaSH.GHC.Compat.TyCon      (isSuperKindTyCon)
-- import           Coercion                    (coercionKind, coercionType)
import           Coercion                    (coercionType)
import           CoreFVs                     (exprSomeFreeVars)
import           CoreSyn                     (AltCon (..), Bind (..), CoreExpr,
                                              Expr (..), rhssOfAlts)
import           DataCon                     (DataCon, dataConExTyVars,
                                              dataConName, dataConRepArgTys,
                                              dataConTag, dataConTyCon,
                                              dataConUnivTyVars, dataConWorkId,
                                              dataConWrapId_maybe)
import           Id                          (isDataConId_maybe)
import           IdInfo                      (IdDetails (..))
import           Literal                     (Literal (..))
import           Module                      (moduleName, moduleNameString)
import           Name                        (Name, nameModule_maybe,
                                              nameOccName, nameUnique)
import           OccName                     (occNameString)
-- import           Pair                        (Pair (..))
import           TyCon                       (AlgTyConRhs (..), PrimRep (..),
                                              SynTyConRhs (..), TyCon,
                                              algTyConRhs, isAlgTyCon,
                                              isFunTyCon, isNewTyCon,
                                              isPrimTyCon,
                                              isSynTyCon, isTupleTyCon,
                                              synTyConRhs_maybe, tyConArity,
                                              tyConDataCons, tyConKind,
                                              tyConName, tyConPrimRep,
                                              tyConUnique)
import           Type                        (tcView)
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
              -> HashMap C.TyConName C.TyCon
makeAllTyCons hm = go hm hm
  where
    go old new
        | HSM.null (new ^. tyConMap) = HSM.empty
        | otherwise                  = tcm `HSM.union` tcm'
      where
        (tcm,old') = State.runState (T.mapM makeTyCon (new ^. tyConMap)) old
        tcm'       = go old' (old' & tyConMap %~ (`HSM.difference` (old ^. tyConMap)))

makeTyCon :: TyCon
          -> State GHC2CoreState C.TyCon
makeTyCon tc = tycon
  where
    tycon
      | isTupleTyCon tc     = mkTupleTyCon
      | isAlgTyCon tc       = mkAlgTyCon
      | isSynTyCon tc       = mkVoidTyCon
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
            Nothing -> return (C.PrimTyCon tcName tcKind tcArity C.VoidRep)

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
            , C.primTyConRep = coreToPrimRep (tyConPrimRep tc)
            }

        mkSuperKindTyCon = do
          tcName <- coreToName tyConName tyConUnique qualfiedNameString tc
          return C.SuperKindTyCon
                   { C.tyConName = tcName
                   }

        mkVoidTyCon = do
          tcName <- coreToName tyConName tyConUnique qualfiedNameString tc
          tcKind <- coreToType (tyConKind tc)
          return (C.PrimTyCon tcName tcKind tcArity C.VoidRep)

        coreToPrimRep :: PrimRep
                      -> C.PrimRep
        coreToPrimRep p = case p of
          VoidRep -> C.VoidRep
          IntRep  -> C.IntRep
          AddrRep -> C.VoidRep
          PtrRep  -> C.VoidRep
          WordRep -> C.VoidRep
          DoubleRep -> C.VoidRep
          _ -> error $ $(curLoc) ++ "Can't convert PrimRep: " ++ showPpr p ++ " in tycon: " ++ showPpr tc

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
    term (Case e b _ alts) = do
     let usesBndr = any ( not . isEmptyVarSet . exprSomeFreeVars (`elem` [b]))
                  $ rhssOfAlts alts
     b' <- coreToId b
     e' <- term e
     let caseTerm v = C.Case v <$> mapM alt alts
     if usesBndr
      then do
        ct <- caseTerm (C.Var (unembed $ C.varType b') (C.varName b'))
        return $ C.Letrec $ bind (rec [(b',embed e')]) ct
      else caseTerm e'

    -- term j@(Cast e co)     = traceIf True ("CAST: " ++ showPpr j) $ case coercionKind co of
    --                            Pair ty1 ty2 -> do
    --                              ty1C <- coreToType ty1
    --                              ty2C <- coreToType ty2
    --                              eC   <- term e
    --                              let caTy = C.mkFunTy ty1C ty2C
    --                                  ca   = C.Prim (string2Name "_CAST_") caTy
    --                              return (C.App ca eC)
    term (Cast e _)        = term e
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
              | f == pack "CLaSH.Signal.mapSignal" -> return (mapSyncTerm xType)
              | f == pack "CLaSH.Signal.appSignal" -> return (mapSyncTerm xType)
              | f == pack "CLaSH.Signal.signal"    -> return (syncTerm xType)
              | f == pack "CLaSH.Signal.pack"      -> return (splitCombineTerm False xType)
              | f == pack "CLaSH.Signal.unpack"    -> return (splitCombineTerm True xType)
              | f == pack "GHC.Base.$"             -> return (dollarAppTerm xType)
              | otherwise                          -> return (C.Prim xNameS xType)
            Just (BlackBox {}) ->
              return (C.Prim xNameS xType)
            Nothing
              | x `elem` unlocs -> return (C.Prim xNameS xType)
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
coreToType' (TyConApp (synTyConRhs_maybe -> Just (SynonymTyCon ty)) args) =
  foldl C.AppTy <$> coreToType ty <*> mapM coreToType args
coreToType' (TyConApp tc args)
  | isFunTyCon tc = foldl C.AppTy (C.ConstTy C.Arrow) <$> mapM coreToType args
  | otherwise     = do tcName <- coreToName tyConName tyConUnique qualfiedNameString tc
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

mapSyncTerm :: C.Type
            -> C.Term
mapSyncTerm (C.ForAllTy tvATy) =
  let (aTV,bTV,C.tyView -> C.FunTy _ (C.tyView -> C.FunTy aTy bTy)) = runFreshM $ do
                { (aTV',C.ForAllTy tvBTy) <- unbind tvATy
                ; (bTV',funTy)            <- unbind tvBTy
                ; return (aTV',bTV',funTy) }
      fName = string2Name "f"
      xName = string2Name "x"
      fTy = C.mkFunTy aTy bTy
      fId = C.Id fName (embed fTy)
      xId = C.Id xName (embed aTy)
  in C.TyLam $ bind aTV $
     C.TyLam $ bind bTV $
     C.Lam   $ bind fId $
     C.Lam   $ bind xId $
     C.App (C.Var fTy fName) (C.Var aTy xName)

mapSyncTerm ty = error $ $(curLoc) ++ show ty

syncTerm :: C.Type
         -> C.Term
syncTerm (C.ForAllTy tvTy) =
  let (aTV,C.tyView -> C.FunTy _ aTy) = runFreshM $ unbind tvTy
      xName = string2Name "x"
      xId = C.Id xName (embed aTy)
  in C.TyLam $ bind aTV $
     C.Lam   $ bind xId $
     C.Var   aTy xName

syncTerm ty = error $ $(curLoc) ++ show ty

splitCombineTerm :: Bool
                 -> C.Type
                 -> C.Term
splitCombineTerm b (C.ForAllTy tvTy) =
  let (aTV,C.tyView -> C.FunTy dictTy (C.tyView -> C.FunTy inpTy outpTy)) = runFreshM $ unbind tvTy
      dictName = string2Name "splitCombineDict"
      xName    = string2Name "x"
      nTy      = if b then inpTy else outpTy
      dId      = C.Id dictName (embed dictTy)
      xId      = C.Id xName    (embed nTy)
      newExpr  = C.TyLam $ bind aTV $
                 C.Lam   $ bind dId $
                 C.Lam   $ bind xId $
                 C.Var nTy xName
  in newExpr

splitCombineTerm _ ty = error $ $(curLoc) ++ show ty

dollarAppTerm :: C.Type
              -> C.Term
dollarAppTerm = mapSyncTerm

isDataConWrapId :: Id -> Bool
isDataConWrapId v = case idDetails v of
                      DataConWrapId {} -> True
                      _                -> False
