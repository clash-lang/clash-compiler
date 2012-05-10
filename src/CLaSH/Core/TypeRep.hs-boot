{-# LANGUAGE MultiParamTypeClasses #-}
module CLaSH.Core.TypeRep where

import Unbound.LocallyNameless

import {-# SOURCE #-} CLaSH.Core.Term

data Type

type Kind = Type
type TyName = Name Type

instance Eq    Type
instance Ord   Type
instance Rep   Type
instance Show  Type
instance Alpha Type
instance Subst Type Type
instance Subst Term Type
