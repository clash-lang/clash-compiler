{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module CLaSH.Core.Term where

import GHC.Generics                     (Generic)
import Unbound.Generics.LocallyNameless (Name)

data Term
type TmName = Name Term

instance Generic Term
