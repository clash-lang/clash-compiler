-- | Capture-free substitution function for CoreHW
module CLaSH.Core.Subst where

import                Unbound.Generics.LocallyNameless (subst, substs)

import                CLaSH.Core.Term         (Term, TmName)
import {-# SOURCE #-} CLaSH.Core.Type         (KiName, Kind, TyName, Type)

-- | Substitutes types in a type
substTys :: [(TyName,Type)]
         -> Type
         -> Type
substTys = substs

-- | Substitutes a type in a type
substTy :: TyName
        -> Type
        -> Type
        -> Type
substTy = subst

-- | Substitutes kinds in a kind
substKindWith :: [(KiName,Kind)]
              -> Kind
              -> Kind
substKindWith = substs

-- | Substitutes a type in a term
substTyInTm :: TyName
            -> Type
            -> Term
            -> Term
substTyInTm = subst

-- | Substitutes types in a term
substTysinTm :: [(TyName,Type)]
             -> Term
             -> Term
substTysinTm = substs

-- | Substitutes a term in a term
substTm :: TmName
        -> Term
        -> Term
        -> Term
substTm = subst

-- | Substitutes terms in a term
substTms :: [(TmName,Term)]
         -> Term
         -> Term
substTms = substs
