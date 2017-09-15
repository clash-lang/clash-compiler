{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                          2017, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Capture-free substitution function for CoreHW
-}

{-# LANGUAGE ViewPatterns #-}

module Clash.Core.Subst where

import Unbound.Generics.LocallyNameless (embed, subst, substs, unembed)

import Clash.Core.Term                  (LetBinding, Term, TmOccName)
import {-# SOURCE #-} Clash.Core.Type   (KiOccName, Kind, TyOccName, Type)

-- | Substitutes types in a type
substTys :: [(TyOccName,Type)]
         -> Type
         -> Type
substTys = substs

-- | Substitutes a type in a type
substTy :: TyOccName
        -> Type
        -> Type
        -> Type
substTy = subst

-- | Substitutes kinds in a kind
substKindWith :: [(KiOccName,Kind)]
              -> Kind
              -> Kind
substKindWith = substs

-- | Substitutes a type in a term
substTyInTm :: TyOccName
            -> Type
            -> Term
            -> Term
substTyInTm = subst

-- | Substitutes types in a term
substTysinTm :: [(TyOccName,Type)]
             -> Term
             -> Term
substTysinTm = substs

-- | Substitutes a term in a term
substTm :: TmOccName
        -> Term
        -> Term
        -> Term
substTm = subst

-- | Substitutes terms in a term
substTms :: [(TmOccName,Term)]
         -> Term
         -> Term
substTms = substs

-- | Substitutes a term in a let-binding
substBndr :: TmOccName -> Term -> LetBinding -> LetBinding
substBndr nm tm (id_,unembed -> tm') = (id_,embed (substTm nm tm tm'))
