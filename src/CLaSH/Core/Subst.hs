module CLaSH.Core.Subst where

import                Unbound.LocallyNameless (subst, substs)

import                CLaSH.Core.Term         (Term, TmName)
import {-# SOURCE #-} CLaSH.Core.Type         (KiName, Kind, TyName, Type)

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

substTyInTm ::
  TyName
  -> Type
  -> Term
  -> Term
substTyInTm = subst

substTysinTm ::
  [(TyName,Type)]
  -> Term
  -> Term
substTysinTm = substs

substTm ::
  TmName
  -> Term
  -> Term
  -> Term
substTm = subst

substTms ::
  [(TmName,Term)]
  -> Term
  -> Term
substTms = substs
