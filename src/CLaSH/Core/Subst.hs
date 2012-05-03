module CLaSH.Core.Subst where

import Unbound.LocallyNameless (subst,substs)

import CLaSH.Core.TypeRep (Type,TyName)

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
