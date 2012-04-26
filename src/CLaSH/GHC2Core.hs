module CLaSH.GHC2Core where

-- External Modules
import Control.Arrow (first,second)
import Unbound.LocallyNameless (Embed(..),Rep,bind,rec,embed)
import qualified Unbound.LocallyNameless as Unbound

-- GHC API
import Coercion   (isCoVar,coercionType)
import CoreFVs    (exprSomeFreeVars)
import CoreSyn    (CoreExpr,Expr (..),Bind(..),AltCon(..),rhssOfAlts)
import DataCon    (DataCon,dataConTag,dataConUnivTyVars,dataConWorkId
                  ,dataConRepArgTys,dataConName,dataConTyCon)
import FastString (unpackFS)
import Id         (isDataConWorkId_maybe)
import Literal    (Literal(..))
import Name       (Name,nameOccName)
import OccName    (occNameString)
import Outputable (showPpr)
import TyCon      (TyCon,AlgTyConRhs(..),isAlgTyCon,isSynTyCon,isTupleTyCon
                  ,isSuperKindTyCon,tyConName,tyConUnique,tyConTyVars,tyConDataCons
                  ,algTyConRhs,isFunTyCon,isNewTyCon)
import Type       (Type,getTyVar_maybe,splitForAllTy_maybe,splitFunTy_maybe
                  ,splitTyConApp_maybe)
import Unique     (Unique,Uniquable(..),getKey)
import Var        (Var,Id,TyVar,varName,varUnique,varType,isTyVar)
import VarSet     (isEmptyVarSet)

-- Local imports
import qualified CLaSH.Core as C

coreToDataCon ::
  DataCon
  -> C.DataCon
coreToDataCon dc =
  C.MkData
    { C.dcName       = coreToName dataConName getUnique dc
    , C.dcTag        = dataConTag dc
    , C.dcRepArgTys  = map coreToType  (dataConRepArgTys  dc)
    , C.dcUnivTyVars = map coreToTyVar (dataConUnivTyVars dc)
    , C.dcWorkId     = coreToId $ dataConWorkId dc
    }

coreToLiteral ::
  Literal
  -> C.Literal
coreToLiteral l = case l of
  MachStr    fs -> C.StringLiteral (unpackFS fs)
  MachInt    i  -> C.IntegerLiteral i
  MachInt64  i  -> C.IntegerLiteral i
  MachWord   i  -> C.IntegerLiteral i
  MachWord64 i  -> C.IntegerLiteral i
  _             -> error $ "Can't convert literal: " ++ show l

coreToTerm ::
  CoreExpr
  -> C.Term
coreToTerm = term
  where
    term (Var x)                 = var x
    term (Lit l)                 = C.Literal (coreToLiteral l)
    term (App eFun (Type tyArg)) = C.TyApp (term eFun) (coreToType tyArg)
    term (App eFun eArg)         = C.App (term eFun) (term eArg)
    term (Lam x e) | isTyVar x   = C.TyLam (bind (coreToTyVar x) (term e))
                   | otherwise   = C.Lam (bind (coreToId x) (term e))
    term (Let (NonRec x e1) e2)  = C.Letrec $ bind
                                      (rec [(coreToId x, embed $ term e1)])
                                      (term e2)
    term (Let (Rec xes) e)       = C.Letrec $ bind
                                      (rec $ map
                                         (first coreToId . second (embed . term))
                                         xes
                                      ) (term e)
    term (Case e b _ alts)       = let usesBndr = not $ all (isEmptyVarSet . exprSomeFreeVars (`elem` [b])) $ rhssOfAlts alts
                                       caseTerm = C.Case (term e) (map alt alts)
                                   in if usesBndr
                                     then C.Letrec $ bind
                                            (rec [(coreToId b, embed $ term e)])
                                            caseTerm
                                     else caseTerm
    term (Cast e _)              = term e
    term (Tick _ e)              = term e
    term (Type _)                = error "Type at non-argument position not supported"
    term (Coercion co)           = C.Prim $ C.PrimCo (coreToType $ coercionType co)

    var x = let xId    = coreToId x
                xNameS = Unbound.name2String $ C.varName xId
            in case (isDataConWorkId_maybe x) of
              Just dc | isNewTyCon (dataConTyCon dc) -> error "Newtype not supported"
                      | otherwise -> if (xNameS `elem` C.primDataCons)
                          then C.Prim (C.PrimCon (coreToDataCon dc))
                          else C.Data (coreToDataCon dc)
              Nothing
                | xNameS `elem` C.primDFuns -> C.Prim (C.PrimDFun xId)
                | xNameS `elem` C.primDicts -> C.Prim (C.PrimDict xId)
                | xNameS `elem` C.primFuns  -> C.Prim (C.PrimFun xId)
                | otherwise -> C.Var (C.varName xId)

    alt (DEFAULT   , _ , e) = bind C.DefaultPat (term e)
    alt (LitAlt l  , _ , e) = bind (C.LitPat $ coreToLiteral l) (term e)
    alt (DataAlt dc, xs, e) = case (as,cs) of
                                ([],[]) -> bind (C.DataPat
                                                  (coreToDataCon dc)
                                                  (map coreToId zs))
                                                (term e)
                                _ -> error "Patterns binding coercions or type variables are not supported"
      where
        (as,ys) = span isTyVar xs
        (cs,zs) = span isCoVar ys

coreToType ::
  Type
  -> C.Type
coreToType ty = case getTyVar_maybe ty of
  Just tv -> C.TyVarTy $ coreToVar tv
  Nothing -> case splitTyConApp_maybe ty of
    Just (tc,args)
      | isFunTyCon tc -> foldl1 C.FunTy (map coreToType args)
      | otherwise     -> C.TyConApp (coreToTyCon tc) (map coreToType args)
    Nothing -> case (splitFunTy_maybe ty) of
      Just (t1,t2) -> C.FunTy (coreToType t1) (coreToType t2)
      Nothing -> case (splitForAllTy_maybe ty) of
        Just (tv,ty') -> C.ForAllTy (bind (coreToTyVar tv) (coreToType ty'))
        Nothing -> error $ "Type application of type variables not supported"

coreToTyCon ::
  TyCon
  -> C.TyCon
coreToTyCon tc
  | isAlgTyCon tc       = algTyCon
  | isSynTyCon tc       = synTyCon
  | isTupleTyCon tc     = tupleTyCon
  | isSuperKindTyCon tc = superKindTyCon
  | otherwise           = error $ "Can't convert TyCon: " ++ showPpr tc
  where
    tcName = coreToName tyConName tyConUnique tc

    algTyCon = C.AlgTyCon
      { C.tyConName   = tcName
      , C.tyConTyVars = map coreToTyVar (tyConTyVars tc)
      , C.algTcRhs    = coreToAlgTyConRhs $ algTyConRhs tc
      }

    synTyCon = C.SynTyCon
      { C.tyConName   = tcName
      , C.tyConTyVars = map coreToTyVar (tyConTyVars tc)
      }

    tupleTyCon = C.TupleTyCon
      { C.tyConName   = tcName
      , C.tyConTyVars = map coreToTyVar (tyConTyVars tc)
      , C.dataCon     = coreToDataCon . head . tyConDataCons $ tc
      }

    superKindTyCon = C.SuperKindTyCon
      { C.tyConName = tcName
      }

coreToAlgTyConRhs ::
  AlgTyConRhs
  -> C.AlgTyConRhs
coreToAlgTyConRhs algTcRhs = case algTcRhs of
  DataTyCon dcs _   -> C.DataTyCon (map coreToDataCon dcs)
  NewTyCon dc _ _ _ -> C.NewTyCon (coreToDataCon dc)
  AbstractTyCon _   -> error $ "Can't convert AlgTyConRhs: AbstractTyCon"
  DataFamilyTyCon   -> error $ "Can't convert AlgTyConRhs: DataFamilyTyCon"

coreToTyVar ::
  TyVar
  -> C.TyVar
coreToTyVar tv =
  C.TyVar
    { C.varName = coreToVar tv
    , C.varKind = Embed $ coreToType (varType tv)
    }

coreToId ::
  Id
  -> C.Id
coreToId i =
  C.Id
    { C.varName = coreToVar i
    , C.varType = Embed $ coreToType (varType i)
    }

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
