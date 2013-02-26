{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -fno-warn-duplicate-constraints #-}

module CLaSH.Core.Type
  ( Type (..)
  , TypeView (..)
  , ConstTy (..)
  , LitTy (..)
  , Kind
  , KiName
  , TyName
  , TyVar
  , ThetaType
  , tyView
  , typeKind
  , mkTyConTy
  , isPredTy
  , noParenPred
  , splitTyAppM
  , mkFunTy
  , mkTyConApp
  , mkForAllTy
  , mkTyVarTy
  , splitFunTy
  , splitTyConAppM
  , isLiftedTypeKind
  , isPolyTy
  , isFunTy
  , applyFunTy
  , applyTy
  )
where

-- External import
import Data.Maybe (isJust)
import Unbound.LocallyNameless as Unbound hiding (Arrow)

-- Local imports
import CLaSH.Core.Subst
import {-# SOURCE #-} CLaSH.Core.Term
import CLaSH.Core.TyCon
import CLaSH.Core.TysPrim
import CLaSH.Core.Var
import CLaSH.Util

data Type
  = VarTy    Kind TyName
  | ConstTy  ConstTy
  | ForAllTy (Bind TyVar Type)
  | AppTy    Type Type
  | LitTy    LitTy
  deriving Show

data TypeView
  = FunTy    Type  Type
  | TyConApp TyCon [Type]
  | OtherType

data ConstTy
  = TyCon TyCon
  | Arrow
  deriving Show

data LitTy
  = NumTy Int
  | SymTy String
  deriving Show

type Kind       = Type
type KindOrType = Type
type TyName     = Name Type
type KiName     = Name Kind

Unbound.derive [''Type,''LitTy,''ConstTy]

instance Alpha Type
instance Alpha ConstTy
instance Alpha LitTy

instance Subst Type LitTy
instance Subst Term LitTy
instance Subst Type ConstTy
instance Subst Term ConstTy
instance Subst Term Type
instance Subst Type Type where
  isvar (VarTy _ v) = Just (SubstName v)
  isvar _           = Nothing

instance Eq Type where
  (==) = aeq

instance Ord Type where
  compare = acompare

tyView :: Type -> TypeView
tyView ty@(AppTy _ _) = case splitTyAppM ty of
  Just (ConstTy Arrow, [ty1,ty2]) -> FunTy ty1 ty2
  Just (ConstTy (TyCon tc), args) -> TyConApp tc args
  _ -> OtherType
tyView (ConstTy (TyCon tc)) = TyConApp tc []
tyView _ = OtherType

mkFunTy :: Type -> Type -> Type
mkFunTy t1 t2 = AppTy (AppTy (ConstTy Arrow) t1) t2

mkTyConApp :: TyCon -> [Type] -> Type
mkTyConApp tc = foldl AppTy (ConstTy $ TyCon tc)

mkTyConTy :: TyCon -> Type
mkTyConTy ty = ConstTy $ TyCon ty

mkForAllTy :: TyVar -> Type -> Type
mkForAllTy tv t = ForAllTy $ bind tv t

type PredType  = Type
type ThetaType = [PredType]

noParenPred :: PredType -> Bool
noParenPred p = isClassPred p || isEqPred p

isClassPred :: PredType -> Bool
isClassPred ty = case tyConAppTyCon_maybe ty of
    Just tyCon | isClassTyCon tyCon -> True
    _                               -> False

isEqPred :: PredType -> Bool
isEqPred ty = case tyConAppTyCon_maybe ty of
    Just tyCon -> (name2Integer $ tyConName tyCon) == eqTyConKey
    _          -> False

tyConAppTyCon_maybe :: Type -> Maybe TyCon
tyConAppTyCon_maybe (tyView -> TyConApp tc _) = Just tc
tyConAppTyCon_maybe _                         = Nothing

isPredTy :: Type -> Bool
isPredTy ty
  | isSuperKind ty = False
  | otherwise      = typeKind ty == constraintKind

isSuperKind :: Type -> Bool
isSuperKind (ConstTy (TyCon skc)) = isSuperKindTyCon skc
isSuperKind _                     = False

isLiftedTypeKind :: Kind -> Bool
isLiftedTypeKind (ConstTy (TyCon ltk)) = (name2Integer (tyConName ltk)) == liftedTypeKindTyConKey
isLiftedTypeKind _                     = False

typeKind :: Type -> Kind
typeKind (VarTy k _)          = k
typeKind (ConstTy Arrow)      = mkFunTy liftedTypeKind liftedTypeKind
typeKind (ConstTy (TyCon tc)) = tyConKind tc
typeKind (ForAllTy b)         = let (_,ty) = runFreshM $ unbind b
                                in typeKind ty
typeKind (AppTy fun arg)      = kindFunResult (typeKind fun) arg
typeKind (LitTy (NumTy _))    = typeNatKind
typeKind (LitTy (SymTy _))    = typeSymbolKind

kindFunResult :: Kind -> KindOrType -> Kind
kindFunResult (tyView -> FunTy _ res) _ = res
kindFunResult (ForAllTy b) arg          = let (kv,ki) = runFreshM . unbind $ b
                                          in substKindWith (zip [varName kv] [arg]) ki
kindFunResult k tys                     = error $ $(curLoc) ++ ("kindFunResult: ") ++ show (k,tys)

constraintKind :: Kind
constraintKind = kindTyConType constraintKindTyCon

isPolyTy :: Type -> Bool
isPolyTy (ForAllTy _)            = True
isPolyTy (tyView -> FunTy _ res) = isPolyTy res
isPolyTy _                       = False

mkTyVarTy ::
  Kind
  -> TyName
  -> Type
mkTyVarTy = VarTy

splitFunTy ::
  Type
  -> Maybe (Type, Type)
splitFunTy (tyView -> FunTy arg res) = Just (arg,res)
splitFunTy (tyView -> TyConApp tc [tyView -> FunTy arg res])
  | name2String (tyConName tc) == "CLaSH.Signal.Sync"
  = Just (arg, mkTyConApp tc [res])
splitFunTy _               = Nothing

isFunTy ::
  Type
  -> Bool
isFunTy = isJust . splitFunTy

applyFunTy ::
  Type
  -> Type
  -> Type
applyFunTy (tyView -> FunTy _ resTy) _ = resTy
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

splitTyConAppM ::
  Type
  -> Maybe (TyCon,[Type])
splitTyConAppM (tyView -> TyConApp tc args) = Just (tc,args)
splitTyConAppM _                            = Nothing

splitTyAppM ::
  Type
  -> Maybe (Type, [Type])
splitTyAppM ty = fmap (second reverse) $ splitTyAppM' [] ty
  where
    splitTyAppM' args (AppTy ty1 ty2) = case splitTyAppM' args ty1 of
                                          Nothing             -> Just (ty1,ty2:args)
                                          Just (ty1',ty1args) -> Just (ty1',ty2:ty1args )
    splitTyAppM' _ _                  = Nothing


