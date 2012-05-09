module CLaSH.Core.Subst where

import Unbound.LocallyNameless (subst,substs)

import CLaSH.Core.TypeRep (Type,Kind,TyName,KiName)

substTys ::
  [(TyName,Type)]
  -> Type
  -> Type
substTys = substs

substTy ::
  TyName
  -> Type
  -> Type
  -> Type
substTy = subst

substKindWith ::
  [(KiName,Kind)]
  -> Kind
  -> Kind
substKindWith = substs
