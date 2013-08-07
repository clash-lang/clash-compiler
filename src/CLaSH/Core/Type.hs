{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
{-# OPTIONS_GHC -fno-warn-duplicate-constraints #-}
#endif

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
  , coreView
  , typeKind
  , mkTyConTy
  , isDictType
  , splitTyAppM
  , mkFunTy
  , mkTyConApp
  , mkForAllTy
  , mkTyVarTy
  , splitFunTy
  , splitTyConAppM
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
  | OtherType Type
  deriving Show

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
  _ -> OtherType ty
tyView (ConstTy (TyCon tc)) = TyConApp tc []
tyView t = OtherType t

coreView :: Type -> TypeView
coreView ty =
  let tView = tyView ty
  in  case tyView ty of
        TyConApp (AlgTyCon {algTcRhs = (NewTyCon _ nt)}) args      -> coreView (newTyConInstRhs nt args)
        TyConApp tc args
          | (name2String $ tyConName tc) == "CLaSH.Signal.Signal"  -> coreView (head args)
          | (name2String $ tyConName tc) == "CLaSH.Signal.SignalP" -> coreView (head args)
        _ -> tView

newTyConInstRhs :: ([TyName],Type) -> [Type] -> Type
newTyConInstRhs (tvs,ty) tys = foldl AppTy (substTys (zip tvs tys1) ty) tys2
  where
    (tys1, tys2) = splitAtList tvs tys

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

isDictType :: PredType -> Bool
isDictType ty = case tyConAppTyCon_maybe ty of
    Just (AlgTyCon {isDictTyCon = d}) -> d
    _                                 -> False

tyConAppTyCon_maybe :: Type -> Maybe TyCon
tyConAppTyCon_maybe (tyView -> TyConApp tc _) = Just tc
tyConAppTyCon_maybe _                         = Nothing

isSuperKind :: Type -> Bool
isSuperKind (ConstTy (TyCon skc)) = isSuperKindTyCon skc
isSuperKind _                     = False

typeKind :: Type -> Kind
typeKind (VarTy k _)          = k
typeKind (ForAllTy b)         = let (_,ty) = runFreshM $ unbind b
                                in typeKind ty
typeKind (LitTy (NumTy _))    = typeNatKind
typeKind (LitTy (SymTy _))    = typeSymbolKind
typeKind (tyView -> FunTy _arg res)
  | isSuperKind k = k
  | otherwise     = liftedTypeKind
  where k = typeKind res

typeKind (tyView -> TyConApp tc args) = kindAppResult (tyConKind tc) args

typeKind (AppTy fun arg)      = kindFunResult (typeKind fun) arg
typeKind (ConstTy ct)         = error $ $(curLoc) ++ "typeKind: naked ConstTy: " ++ show ct

kindAppResult :: Kind -> [Type] -> Kind
kindAppResult k []     = k
kindAppResult k (a:as) = kindAppResult (kindFunResult k a) as

kindFunResult :: Kind -> KindOrType -> Kind
kindFunResult (tyView -> FunTy _ res) _ = res
kindFunResult (ForAllTy b) arg          = let (kv,ki) = runFreshM . unbind $ b
                                          in substKindWith (zip [varName kv] [arg]) ki
kindFunResult k tys                     = error $ $(curLoc) ++ ("kindFunResult: ") ++ show (k,tys)

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
splitFunTy (coreView -> FunTy arg res) = Just (arg,res)
splitFunTy _                          = Nothing

isFunTy ::
  Type
  -> Bool
isFunTy = isJust . splitFunTy

applyFunTy ::
  Type
  -> Type
  -> Type
applyFunTy (coreView -> FunTy _ resTy) _ = resTy
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
