{-# LANGUAGE MultiParamTypeClasses #-}
module CLaSH.Core.Type where

import Unbound.LocallyNameless

import {-# SOURCE #-} CLaSH.Core.Term

data Type
type Kind = Type

instance Eq    Type
instance Rep   Type
instance Show  Type
instance Alpha Type
instance Subst Type Type
instance Subst Term Type
