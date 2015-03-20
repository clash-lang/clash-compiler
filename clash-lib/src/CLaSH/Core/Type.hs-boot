{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module CLaSH.Core.Type where

import Control.DeepSeq
import GHC.Generics
import Unbound.Generics.LocallyNameless

import {-# SOURCE #-} CLaSH.Core.Term
import {-# SOURCE #-} CLaSH.Core.TyCon

data Type

type Kind   = Type
type TyName = Name Type
type KiName = Name Kind

instance Eq       Type
instance Generic  Type
instance Show     Type
instance Alpha    Type
instance Subst    Type Type
instance Subst    Term Type
instance NFData   Type

mkTyConTy :: TyConName -> Type
