module CLaSH.Core.Type
  ( Type
  , Kind
  , SuperKind
  , KindOrType
  , PredType
  , ThetaType
  , TyName
  , TyVar
  , Delta
  , mkFunTy
  , mkForAllTy
  , mkTyVarTy
  , splitFunTy
  , noParenPred
  , isPredTy
  , isLiftedTypeKind
  , isPolyTy
  , isFunTy
  , applyFunTy
  , applyTy
  )
where

-- External import
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe (fromMaybe,isJust)
import Unbound.LocallyNameless (Fresh,bind,runFreshM,unbind,name2Integer,unembed)

-- Local imports
import CLaSH.Core.Subst
import CLaSH.Core.TyCon
import CLaSH.Core.TypeRep
import CLaSH.Core.TysPrim
import CLaSH.Core.Var
import CLaSH.Util

type KindOrType = Type
type PredType   = Type
type ThetaType  = [PredType]

type Delta = HashMap.HashMap TyName Kind

mkFunTy :: Type -> Type -> Type
mkFunTy t1 t2 = FunTy t1 t2

mkForAllTy :: TyVar -> Type -> Type
mkForAllTy tv t = ForAllTy $ bind tv t

noParenPred :: PredType -> Bool
noParenPred p = isClassPred p || isEqPred p

isClassPred, isEqPred :: PredType -> Bool
isClassPred ty = case tyConAppTyCon_maybe ty of
    Just tyCon | isClassTyCon tyCon -> True
    _                               -> False

isEqPred ty = case tyConAppTyCon_maybe ty of
    Just tyCon -> (name2Integer $ tyConName tyCon) == eqTyConKey
    _          -> False

tyConAppTyCon_maybe :: Type -> Maybe TyCon
tyConAppTyCon_maybe (TyConApp tc _) = Just tc
tyConAppTyCon_maybe _               = Nothing

isPredTy :: Delta -> Type -> Bool
isPredTy d ty
  | isSuperKind ty = False
  | otherwise      = typeKind d ty == constraintKind

isSuperKind :: Type -> Bool
isSuperKind (TyConApp skc []) = isSuperKindTyCon skc
isSuperKind _                 = False

isLiftedTypeKind :: Kind -> Bool
isLiftedTypeKind (TyConApp tc []) = (name2Integer (tyConName tc)) == liftedTypeKindTyConKey
isLiftedTypeKind _                = False

typeKind :: Delta -> Type -> Kind
typeKind _ (TyConApp tc tys) = kindAppResult (tyConKind tc) tys
typeKind d (ForAllTy b)      = let (tv,ty) = runFreshM $ unbind b
                                   d'  = HashMap.insert
                                           (varName tv)
                                           (unembed $ varKind tv)
                                           d
                               in  typeKind d' ty
typeKind d (TyVarTy tv)      = fromMaybe (error $ $(curLoc) ++ "typeKind: " ++ show tv)
                             $ HashMap.lookup tv d
typeKind d (FunTy _arg res)
  | isSuperKind k = k
  | otherwise     = liftedTypeKind
  where
    k = typeKind d res

kindAppResult :: Kind -> [Type] -> Kind
kindAppResult k []     = k
kindAppResult k (a:as) = kindAppResult (kindFunResult k a) as

kindFunResult :: Kind -> KindOrType -> Kind
kindFunResult (FunTy _ res) _  = res
kindFunResult (ForAllTy b) arg = let (kv,ki) = runFreshM . unbind $ b
                                 in substKindWith (zip [varName kv] [arg]) ki
kindFunResult _ _              = error $ $(curLoc) ++ "kindFunResult"

constraintKind :: Kind
constraintKind = kindTyConType constraintKindTyCon

isPolyTy :: Type -> Bool
isPolyTy (ForAllTy _)  = True
isPolyTy (FunTy _ res) = isPolyTy res
isPolyTy _             = False

mkTyVarTy ::
  TyName
  -> Type
mkTyVarTy = TyVarTy

splitFunTy ::
  Type
  -> Maybe (Type, Type)
splitFunTy (FunTy arg res) = Just (arg,res)
splitFunTy _               = Nothing

isFunTy ::
  Type
  -> Bool
isFunTy = isJust . splitFunTy

applyFunTy ::
  Type
  -> Type
  -> Type
applyFunTy (FunTy _ resTy) _ = resTy
applyFunTy _ _ = error $ $(curLoc) ++ "Report as bug: not a FunTy"

applyTy ::
  Fresh m
  => Type
  -> KindOrType
  -> m Type
applyTy (ForAllTy b) arg = do
  (tv,ty) <- unbind b
  return $ substTy (varName tv) arg ty
applyTy _ _ = error $ $(curLoc) ++ "applyTy: not a forall type"
