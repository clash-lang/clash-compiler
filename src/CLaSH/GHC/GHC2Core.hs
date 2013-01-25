{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module CLaSH.GHC.GHC2Core
  ( makeAllTyDataCons
  , coreToTerm
  , coreToBndr
  , coreToPrimBndr
  , unlocatable
  )
where

-- External Modules
import Control.Monad.Reader               (Reader)
import qualified Control.Monad.Reader     as Reader
import Control.Monad.State                (StateT,lift)
import qualified Control.Monad.State.Lazy as State
import Data.ByteString.Lazy.Char8         (pack)
import Data.Hashable                      (Hashable(..),hash)
import Data.HashMap.Lazy                  (HashMap)
import qualified Data.HashMap.Lazy        as HashMap
import Data.Label.PureM                   as LabelM
import Data.Maybe                         (fromMaybe)
import Unbound.LocallyNameless            (Rep,bind,rec,embed)
import qualified Unbound.LocallyNameless  as Unbound

-- GHC API
import BasicTypes (TupleSort (..))
import Coercion   (isCoVar,coercionType)
import CoreFVs    (exprSomeFreeVars)
import CoreSyn    (CoreExpr,Expr (..),Bind(..),AltCon(..),rhssOfAlts)
import DataCon    (DataCon,dataConTag,dataConUnivTyVars,dataConWorkId,
  dataConRepArgTys,dataConName,dataConTyCon)
import CLaSH.GHC.Compat.FastString (unpackFS,unpackFB)
import Id         (isDataConWorkId_maybe)
import Literal    (Literal(..))
import Module     (moduleName,moduleNameString)
import Name       (Name,nameOccName,nameModule_maybe)
import OccName    (occNameString)
import CLaSH.GHC.Compat.Outputable (showPpr)
import TyCon      (TyCon,AlgTyConRhs(..),TyConParent(..),PrimRep(..),
  isAlgTyCon,isTupleTyCon,tyConName,tyConUnique,tyConTyVars,
  tyConDataCons,algTyConRhs,isFunTyCon,isNewTyCon,tyConKind,tyConArity,
  tyConParent,isSynTyCon,isPrimTyCon,tyConPrimRep,isAbstractTyCon)
import CLaSH.GHC.Compat.TyCon (isSuperKindTyCon)
import Type       (tcView)
import TypeRep    (Type(..),TyLit(..))
import TysWiredIn (tupleTyCon)
import Unique     (Unique,Uniquable(..),getKey)
import Var        (Var,Id,TyVar,varName,varUnique,varType,isTyVar)
import VarSet     (isEmptyVarSet)

-- Local imports
import qualified CLaSH.Core.DataCon as C
import qualified CLaSH.Core.Literal as C
import qualified CLaSH.Core.Prim    as C
import qualified CLaSH.Core.Term    as C
import qualified CLaSH.Core.TyCon   as C
import qualified CLaSH.Core.TypeRep as C
import qualified CLaSH.Core.Util    as C
import qualified CLaSH.Core.Var     as C
import CLaSH.Primitives.Types
import CLaSH.Util

instance Show TyCon where
  show tc = showPpr tc

type R    = Reader GHC2CoreState
type SR a = StateT GHC2CoreState R a

data GHC2CoreState
  = GHC2CoreState
  { _tyConMap    :: HashMap TyCon   C.TyCon
  , _dataConMap  :: HashMap DataCon C.DataCon
  , _unlocatable :: [Var]
  }

mkLabels [''GHC2CoreState]

instance Hashable TyCon where
  hashWithSalt s = hashWithSalt s . getKey . getUnique

instance Hashable DataCon where
  hashWithSalt s dc = hashWithSalt s ((dataConTag dc) `hashWithSalt` (hash $ dataConTyCon dc))

makeAllTyDataCons :: [TyCon] -> GHC2CoreState
makeAllTyDataCons tyCons =
  let s = Reader.runReader (State.execStateT
                              (mapM makeTyCon (tyCons' ++ tupleTyCons))
                              emptyState)
                           s
  in  s
  where
    emptyState = GHC2CoreState HashMap.empty HashMap.empty []
    tyCons'    = filter (\tc -> not (isSynTyCon tc || isAbstractTyCon tc))
                  tyCons
    tupleTyCons = concat [ map (`tupleTyCon` x)
                            [BoxedTuple,UnboxedTuple,ConstraintTuple]
                         | x <- [2..62]
                         ]

makeTyCon ::
  TyCon
  -> SR ()
makeTyCon tc = do
    tycon' <- tycon
    LabelM.modify tyConMap (HashMap.insert tc tycon')
  where
    tycon
      | isAlgTyCon tc       = mkAlgTyCon
      | isTupleTyCon tc     = mkTupleTyCon
      | isSynTyCon tc       = error $ $(curLoc) ++ "Can't convert SynTyCon: "
                                                ++ showPpr tc
      | isPrimTyCon tc      = mkPrimTyCon
      | isSuperKindTyCon tc = return mkSuperKindTyCon
      | otherwise           = error $ $(curLoc) ++ "Can't convert TyCon: "
                                                ++ showPpr tc
      where
        tcName  = coreToName tyConName tyConUnique qualfiedNameString tc
        tcArity = tyConArity tc

        mkAlgTyCon = do
          tcKind <- lift $ coreToType (tyConKind tc)
          tcRhs  <- makeAlgTyConRhs tc $ algTyConRhs tc
          return $
            C.AlgTyCon
            { C.tyConName   = tcName
            , C.tyConKind   = tcKind
            , C.tyConArity  = tcArity
            , C.tyConTyVars = map coreToVar (tyConTyVars tc)
            , C.algTcRhs    = tcRhs
            , C.algTcParent = coreToAltTcParent $ tyConParent tc
            }

        mkTupleTyCon = do
          tcKind <- lift $ coreToType (tyConKind tc)
          tcDc   <- makeDataCon . head . tyConDataCons $ tc
          return $
            C.TupleTyCon
            { C.tyConName   = tcName
            , C.tyConKind   = tcKind
            , C.tyConArity  = tcArity
            , C.tyConTyVars = map coreToVar (tyConTyVars tc)
            , C.dataCon     = tcDc
            }

        mkPrimTyCon = do
          tcKind <- lift $ coreToType (tyConKind tc)
          return $
            C.PrimTyCon
            { C.tyConName    = tcName
            , C.tyConKind    = tcKind
            , C.tyConArity   = tcArity
            , C.primTyConRep = coreToPrimRep (tyConPrimRep tc)
            }

        mkSuperKindTyCon = C.SuperKindTyCon
          { C.tyConName = tcName
          }

makeAlgTyConRhs ::
  TyCon
  -> AlgTyConRhs
  -> SR C.AlgTyConRhs
makeAlgTyConRhs tc algTcRhs = case algTcRhs of
  DataTyCon dcs _   -> C.DataTyCon <$> (mapM makeDataCon dcs)
  NewTyCon dc _ _ _ -> C.NewTyCon <$> (makeDataCon dc)
  AbstractTyCon _   -> error $ $(curLoc) ++ "Can't convert AlgTyConRhs: AbstractTyCon of: " ++ showPpr tc
  DataFamilyTyCon   -> return C.DataFamilyTyCon -- error $ $(curLoc) ++ "Can't convert AlgTyConRhs: DataFamilyTyCon: " ++ showPpr tc

makeDataCon ::
  DataCon
  -> SR C.DataCon
makeDataCon dc = do
  repTys   <- lift $ mapM coreToType (dataConRepArgTys  dc)
  workIdTy <- lift $ coreToType (varType $ dataConWorkId dc)
  let dataCon =
        C.MkData
          { C.dcName       = coreToName dataConName getUnique nameString dc
          , C.dcTag        = dataConTag dc
          , C.dcRepArgTys  = repTys
          , C.dcUnivTyVars = map coreToVar (dataConUnivTyVars dc)
          , C.dcWorkId     = ( coreToVar $ dataConWorkId dc
                             , workIdTy)
          }
  LabelM.modify dataConMap (HashMap.insert dc dataCon)
  return dataCon

coreToTerm ::
  PrimMap
  -> GHC2CoreState
  -> CoreExpr
  -> C.Term
coreToTerm primMap s coreExpr = Reader.runReader (term coreExpr) s
  where
    term (Var x)                 = var x
    term (Lit l)                 = return $ C.Literal (coreToLiteral l)
    term (App eFun (Type tyArg)) = C.TyApp <$> (term eFun) <*> (coreToType tyArg)
    term (App eFun eArg)         = C.App <$> (term eFun) <*> (term eArg)
    term (Lam x e) | isTyVar x   = C.TyLam <$> (bind <$> (coreToTyVar x) <*> (term e))
                   | otherwise   = C.Lam <$> (bind <$> (coreToId x) <*> (term e))
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

    term (Case e b ty alts) = do
     let usesBndr = any ( not . isEmptyVarSet . exprSomeFreeVars (`elem` [b]))
                  $ rhssOfAlts alts
     b' <- coreToId b
     e' <- term e
     let caseTerm v = C.Case v <$> coreToType ty <*> mapM alt alts
     if usesBndr
      then do
        ct <- caseTerm (C.Var $ C.varName b')
        return $ C.Letrec $ bind (rec [(b',embed e')]) ct
      else caseTerm e'

    term (Cast e _)        = term e
    term (Tick _ e)        = term e
    term (Type _)          = error $ $(curLoc) ++ "Type at non-argument position not supported"
    term (Coercion co)     = C.Prim <$> C.PrimCo <$> (coreToType $ coercionType co)

    var x =
      let xVar   = coreToVar x
          xPrim  = coreToPrimVar x
          xNameS = pack $ Unbound.name2String xPrim
      in do
        unlocs <- LabelM.asks unlocatable
        xType <- coreToType (varType x)
        let mapSyncName = pack
        case (isDataConWorkId_maybe x) of
          Just dc | isNewTyCon (dataConTyCon dc) -> error $ $(curLoc) ++ "Newtype not supported"
                  | otherwise ->  case (HashMap.lookup xNameS primMap) of
                      Just (Primitive _ Constructor) ->
                        C.Prim <$> C.PrimCon <$> (coreToDataCon dc)
                      Just (BlackBox {}) ->
                        return $ C.Prim (C.PrimFun xPrim xType)
                      _ ->
                        C.Data <$> (coreToDataCon dc)
          Nothing -> case HashMap.lookup xNameS primMap of
            Just (Primitive _ DFun) ->
              return $ C.Prim (C.PrimDFun xPrim xType)
            Just (Primitive _ Dictionary) ->
              return $ C.Prim (C.PrimDict xPrim xType)
            Just (Primitive f Function)
              | f == pack "CLaSH.Signal.mapSync" -> return (C.mapSyncTerm xType)
              | f == pack "CLaSH.Signal.appSync" -> return (C.mapSyncTerm xType)
              | f == pack "CLaSH.Signal.sync"    -> return (C.syncTerm xType)
            Just (Primitive _ Function) ->
              return $ C.Prim (C.PrimFun  xPrim xType)
            Just (Primitive _ Constructor) ->
              error $ $(curLoc) ++ "Primitive DataCon not a identified as DataCon"
            Just (BlackBox {}) ->
              return $ C.Prim (C.PrimFun xPrim xType)
            Nothing
              | x `elem` unlocs -> return $ C.Prim (C.PrimFun  xPrim xType)
              | otherwise -> return $ C.Var xVar

    alt (DEFAULT   , _ , e) = bind C.DefaultPat <$> (term e)
    alt (LitAlt l  , _ , e) = bind (C.LitPat . embed $ coreToLiteral l) <$> (term e)
    alt (DataAlt dc, xs, e) = case (as,cs) of
      ([],[]) -> bind <$> (C.DataPat . embed <$>
                            (coreToDataCon dc) <*>
                            (mapM coreToId zs)) <*>
                      (term e)
      _ -> error $ $(curLoc) ++ "Patterns binding coercions or type variables are not supported"
      where
        (as,ys) = span isTyVar xs
        (cs,zs) = span isCoVar ys

coreToDataCon ::
  DataCon
  -> R C.DataCon
coreToDataCon dc = fmap ( fromMaybe (error $ "DataCon: " ++ showPpr dc ++ " not found")
                        . HashMap.lookup dc
                        ) $ LabelM.asks dataConMap

coreToLiteral ::
  Literal
  -> C.Literal
coreToLiteral l = case l of
  MachStr    fs  -> C.StringLiteral (unpackFB fs)
  MachInt    i   -> C.IntegerLiteral i
  MachInt64  i   -> C.IntegerLiteral i
  MachWord   i   -> C.IntegerLiteral i
  MachWord64 i   -> C.IntegerLiteral i
  LitInteger i _ -> C.IntegerLiteral i
  _              -> error $ $(curLoc) ++ "Can't convert literal: " ++ showPpr l

coreToType ::
  Type
  -> R C.Type
coreToType ty = coreToType' $ fromMaybe ty (tcView ty)

coreToType' ::
  Type
  -> R C.Type
coreToType' (TyVarTy tv) = return $ C.TyVarTy (coreToVar tv)
coreToType' (TyConApp tc args)
  | isFunTyCon tc = foldl1 C.FunTy <$> (mapM coreToType args)
  | otherwise     = C.TyConApp <$> coreToTyCon tc <*> mapM coreToType args
coreToType' (FunTy ty1 ty2)  = C.FunTy <$> coreToType ty1 <*> coreToType ty2
coreToType' (ForAllTy tv ty) = C.ForAllTy <$>
                               (bind <$> coreToTyVar tv <*> coreToType ty)
coreToType' (LitTy tyLit)    = return $ C.LitTy (coreToTyLit tyLit)
coreToType' ty@(AppTy _ _)   = error $ $(curLoc) ++ "Type application of type variables not supported" ++ showPpr ty

coreToTyLit ::
  TyLit
  -> C.TyLit
coreToTyLit (NumTyLit i) = C.NumTyLit (fromInteger i)
coreToTyLit (StrTyLit s) = C.StrTyLit (unpackFS s)

coreToTyCon ::
  TyCon
  -> R C.TyCon
coreToTyCon tc = fmap ( fromMaybe (error $ "TyCon: " ++ showPpr tc ++ " not found")
                      . HashMap.lookup tc
                      ) $ LabelM.asks tyConMap

coreToAltTcParent ::
  TyConParent
  -> C.TyConParent
coreToAltTcParent algTcParent = case algTcParent of
  NoParentTyCon -> C.NoParentTyCon
  ClassTyCon _  -> C.ClassTyCon
  _             -> error $ $(curLoc) ++ "Can't convert algTcParent: " ++ showPpr algTcParent

coreToPrimRep ::
  PrimRep
  -> C.PrimRep
coreToPrimRep p = case p of
  VoidRep -> C.VoidRep
  IntRep  -> C.IntRep
  AddrRep -> C.AddrRep
  PtrRep  -> C.PtrRep
  _ -> error $ $(curLoc) ++ "Can't convert PrimRep: " ++ showPpr p

coreToTyVar ::
  TyVar
  -> R C.TyVar
coreToTyVar tv =
  C.TyVar (coreToVar tv) <$> (embed <$> coreToType (varType tv))

coreToBndr ::
  GHC2CoreState
  -> Id
  -> C.Id
coreToBndr s bndr = Reader.runReader (coreToId bndr) s

coreToPrimBndr ::
  GHC2CoreState
  -> Id
  -> C.Id
coreToPrimBndr s bndr = Reader.runReader (coreToPrimId bndr) s

coreToPrimId ::
  Id
  -> R C.Id
coreToPrimId i =
  C.Id (coreToPrimVar i) <$> (embed <$> coreToType (varType i))

coreToId ::
  Id
  -> R C.Id
coreToId i =
  C.Id (coreToVar i) <$> (embed <$> coreToType (varType i))

coreToVar ::
  Rep a
  => Var
  -> Unbound.Name a
coreToVar = coreToName varName varUnique nameString

coreToPrimVar ::
  Var
  -> Unbound.Name C.Term
coreToPrimVar = coreToName varName varUnique qualfiedNameString

coreToName ::
  Rep a
  => (b -> Name)
  -> (b -> Unique)
  -> (Name -> String)
  -> b
  -> Unbound.Name a
coreToName toName toUnique toString v =
  Unbound.makeName
    (toString $ toName v)
    (toInteger . getKey . toUnique $ v)

nameString ::
  Name
  -> String
nameString = occNameString . nameOccName

qualfiedNameString ::
  Name
  -> String
qualfiedNameString n = fromMaybe "_INTERNAL_" modName ++ ('.':occName)
  where
    modName = do
      module_ <- nameModule_maybe n
      let moduleNm = moduleName module_
      return (moduleNameString moduleNm)

    occName = occNameString $ nameOccName n
