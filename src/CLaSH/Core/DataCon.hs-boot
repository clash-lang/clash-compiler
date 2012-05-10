{-# LANGUAGE MultiParamTypeClasses #-}
module CLaSH.Core.DataCon where

import Unbound.LocallyNameless

import {-# SOURCE #-} CLaSH.Core.Term (Term)
import {-# SOURCE #-} CLaSH.Core.TypeRep (Type)

data DataCon

instance Eq    DataCon
instance Ord   DataCon
instance Rep   DataCon
instance Show  DataCon
instance Alpha DataCon
instance Subst Type DataCon
instance Subst Term DataCon
