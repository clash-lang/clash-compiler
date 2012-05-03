{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CLaSH.Core.Type
  ( Type
  , Kind
  , KindOrType
  , TyName
  , TyVar
  , mkFunTy
  , mkForAllTy
  , applyTy
  , splitFunTy_maybe
  )
where

-- External import
import Control.Arrow           (first)
import Unbound.LocallyNameless (bind,runFreshM,unbind)

-- Local imports
import CLaSH.Core.Subst
import CLaSH.Core.TyCon
import CLaSH.Core.TypeRep
import CLaSH.Core.Var

mkFunTy :: Type -> Type -> Type
mkFunTy t1 t2 = FunTy t1 t2

mkForAllTy :: TyVar -> Type -> Type
mkForAllTy tv t = ForAllTy $ bind tv t

mkTyConApp :: TyCon -> [Type] -> Type
mkTyConApp tycon tys = TyConApp tycon tys

mkAppTys :: Type -> [Type] -> Type
mkAppTys orig_ty1 [] = orig_ty1
mkAppTys orig_ty1 orig_tys2
  = mk_app orig_ty1
  where
    mk_app (TyConApp tc tys) = mkTyConApp tc (tys ++ orig_tys2)
    mk_app _                 = error $ "mkAppTys: not a TyConApp"

splitFunTy_maybe ::
  Type
  -> Maybe (Type,Type)
splitFunTy_maybe ty | Just ty' <- coreView ty = splitFunTy_maybe ty'
splitFunTy_maybe (FunTy arg res) = Just (arg,res)
splitFunTy_maybe _               = Nothing

coreView :: Type -> Maybe Type
coreView (TyConApp tc tys) | Just (tenv, rhs, tys') <- coreExpandTyCon_maybe tc tys
                           = let substEnv = map (first varName) tenv
                             in Just (mkAppTys (substTys substEnv rhs) tys')
coreView _                 = Nothing

applyTy ::
  Type
  -> KindOrType
  -> Type
applyTy ty arg | Just ty' <- coreView ty = applyTy ty' arg
applyTy (ForAllTy b) arg = let (tv,ty) = runFreshM . unbind $ b
                           in substTy (varName tv) arg ty
applyTy _ _ = error "applyTy: not a forall type"
