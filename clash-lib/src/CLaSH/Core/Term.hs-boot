{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module CLaSH.Core.Term where

import GHC.Generics                     (Generic)
import Unbound.Generics.LocallyNameless (Alpha,Name,Subst)

data Term

type TmName = Name Term

instance Generic Term
instance Show    Term
instance Alpha   Term
instance Subst   Term Term
