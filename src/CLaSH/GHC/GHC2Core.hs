{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module CLaSH.GHC.GHC2Core
  ( makeAllTyDataCons
  , coreToTerm
  , coreToBndr
  , unlocatable
  )
where

-- External Modules
import Control.Monad                        ((<=<))
import Control.Monad.Reader                 (Reader)
import qualified Control.Monad.Reader     as Reader
import Control.Monad.State                  (StateT,lift)
import qualified Control.Monad.State.Lazy as State
import Data.Hashable                        (Hashable(..))
import Data.HashMap.Lazy                    (HashMap)
import qualified Data.HashMap.Lazy       as HashMap
import Data.Label.PureM                  as LabelM
import Data.Maybe                           (fromMaybe)
import Unbound.LocallyNameless              (Rep,bind,rec,embed)
import qualified Unbound.LocallyNameless as Unbound

-- GHC API
import BasicTypes (TupleSort (..))
import Coercion   (isCoVar,coercionType)
import CoreFVs    (exprSomeFreeVars)
import CoreSyn    (CoreExpr,Expr (..),Bind(..),AltCon(..),rhssOfAlts)
import DataCon    (DataCon,dataConTag,dataConUnivTyVars,dataConWorkId,
  dataConRepArgTys,dataConName,dataConTyCon)
import FastString (unpackFS)
import Id         (isDataConWorkId_maybe)
import Literal    (Literal(..))
import Name       (Name,nameOccName)
import OccName    (occNameString)
import Outputable (showPpr)
import TyCon      (TyCon,AlgTyConRhs(..),TyConParent(..),PrimRep(..),
  isAlgTyCon,isTupleTyCon,isSuperKindTyCon,tyConName,tyConUnique,tyConTyVars,
  tyConDataCons,algTyConRhs,isFunTyCon,isNewTyCon,tyConKind,tyConArity,
  tyConParent,isSynTyCon,isPrimTyCon,tyConPrimRep,isAbstractTyCon)
import Type       (Type,getTyVar_maybe,splitForAllTy_maybe,splitFunTy_maybe,
  splitTyConApp_maybe,tcView)
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
import qualified CLaSH.Core.Var     as C
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
  hash = hash . getKey . getUnique

instance Hashable DataCon where
  hash dc = (dataConTag dc) `hashWithSalt` (hash $ dataConTyCon dc)

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
        tcName  = coreToName tyConName tyConUnique tc
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
  DataFamilyTyCon   -> error $ $(curLoc) ++ "Can't convert AlgTyConRhs: DataFamilyTyCon: " ++ showPpr tc

makeDataCon ::
  DataCon
  -> SR C.DataCon
makeDataCon dc = do
  repTys   <- lift $ mapM coreToType (dataConRepArgTys  dc)
  workIdTy <- lift $ coreToType (varType $ dataConWorkId dc)
  let dataCon =
        C.MkData
          { C.dcName       = coreToName dataConName getUnique dc
          , C.dcTag        = dataConTag dc
          , C.dcRepArgTys  = repTys
          , C.dcUnivTyVars = map coreToVar (dataConUnivTyVars dc)
          , C.dcWorkId     = ( coreToVar $ dataConWorkId dc
                             , workIdTy)
          }
  LabelM.modify dataConMap (HashMap.insert dc dataCon)
  return dataCon

coreToTerm ::
  GHC2CoreState
  -> CoreExpr
  -> C.Term
coreToTerm s coreExpr = Reader.runReader (term coreExpr) s
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
                                    return $
                                      C.Letrec $ bind
                                          (rec [(x', embed e1')])
                                          e2'

    term (Let (Rec xes) e) = do
                              xes' <- mapM
                                        ( firstM  coreToId <=<
                                          secondM ((return . embed) <=< term)
                                        ) xes
                              e' <- term e
                              return $
                                C.Letrec $ bind
                                  (rec xes')
                                  e'

    term (Case e b ty alts) = do
                               let usesBndr =
                                      any ( not
                                          . isEmptyVarSet
                                          . exprSomeFreeVars (`elem` [b])
                                          ) $ rhssOfAlts alts
                               b'       <- coreToId b
                               e'       <- term e
                               let caseTerm v = C.Case v
                                                <$> coreToType ty
                                                <*> mapM alt alts
                               if usesBndr
                                then do
                                  ct <- caseTerm (C.Var $ C.varName b')
                                  return $ C.Letrec $ bind
                                        (rec [(b',embed e')])
                                        ct
                                else caseTerm e'

    term (Cast e _)        = term e
    term (Tick _ e)        = term e
    term (Type _)          = error $ $(curLoc) ++ "Type at non-argument position not supported"
    term (Coercion co)     = C.Prim <$> C.PrimCo <$> (coreToType $ coercionType co)

    var x = let xVar   = coreToVar x
                xNameS = Unbound.name2String xVar
            in do
              unlocs <- LabelM.asks unlocatable
              xType <- coreToType (varType x)
              case (isDataConWorkId_maybe x) of
                Just dc | isNewTyCon (dataConTyCon dc) -> error $ $(curLoc) ++ "Newtype not supported"
                        | otherwise -> if (xNameS `elem` C.primDataCons)
                            then C.Prim <$> C.PrimCon <$> (coreToDataCon dc)
                            else C.Data <$> (coreToDataCon dc)
                Nothing
                  | xNameS `elem` C.primDFuns -> return $ C.Prim (C.PrimDFun xVar xType)
                  | xNameS `elem` C.primDicts -> return $ C.Prim (C.PrimDict xVar xType)
                  | xNameS `elem` C.primFuns  -> return $ C.Prim (C.PrimFun  xVar xType)
                  | x `elem` unlocs -> return $ C.Prim (C.PrimFun xVar xType)
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
  MachStr    fs  -> C.StringLiteral (unpackFS fs)
  MachInt    i   -> C.IntegerLiteral i
  MachInt64  i   -> C.IntegerLiteral i
  MachWord   i   -> C.IntegerLiteral i
  MachWord64 i   -> C.IntegerLiteral i
  LitInteger i _ -> C.IntegerLiteral i
  _              -> error $ $(curLoc) ++ "Can't convert literal: " ++ show l

coreToType ::
  Type
  -> R C.Type
coreToType ty = coreToType' $ fromMaybe ty (tcView ty)

coreToType' ::
  Type
  -> R C.Type
coreToType' ty = case getTyVar_maybe ty of
  Just tv -> return $ C.TyVarTy (coreToVar tv)
  Nothing -> case splitTyConApp_maybe ty of
    Just (tc,args)
      | isFunTyCon tc -> (foldl1 C.FunTy) <$> (mapM coreToType args)
      | otherwise     -> C.TyConApp <$> (coreToTyCon tc) <*> (mapM coreToType args)
    Nothing -> case (splitFunTy_maybe ty) of
      Just (t1,t2) -> C.FunTy <$> (coreToType t1) <*> (coreToType t2)
      Nothing -> case (splitForAllTy_maybe ty) of
        Just (tv,ty') -> C.ForAllTy <$> (bind <$> (coreToTyVar tv) <*> (coreToType ty'))
        Nothing -> error $ $(curLoc) ++ "Type application of type variables not supported"

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

coreToId ::
  Id
  -> R C.Id
coreToId i =
  C.Id (coreToVar i) <$> (embed <$> coreToType (varType i))

coreToVar ::
  Rep a
  => Var
  -> Unbound.Name a
coreToVar = coreToName varName varUnique

coreToName ::
  Rep a
  => (b -> Name)
  -> (b -> Unique)
  -> b
  -> Unbound.Name a
coreToName toName toUnique v =
  Unbound.makeName
    (nameString $ toName v)
    (toInteger . getKey . toUnique $ v)

nameString ::
  Name
  -> String
nameString = occNameString . nameOccName
