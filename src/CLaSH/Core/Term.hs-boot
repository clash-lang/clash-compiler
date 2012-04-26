module CLaSH.Core.Term where

import Unbound.LocallyNameless

data Term

instance Rep   Term
instance Show  Term
instance Alpha Term
