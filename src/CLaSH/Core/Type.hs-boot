{-# LANGUAGE MultiParamTypeClasses #-}
module CLaSH.Core.Type where

import Unbound.LocallyNameless

import {-# SOURCE #-} CLaSH.Core.Term
import {-# SOURCE #-} CLaSH.Core.TyCon

data Type

type Kind   = Type
type TyName = Name Type
type KiName = Name Kind

instance Eq    Type
instance Ord   Type
instance Rep   Type
instance Show  Type
instance Alpha Type
instance Subst Type Type
instance Subst Term Type

mkTyConTy :: TyCon -> Type
