{-# LANGUAGE MultiParamTypeClasses #-}
module CLaSH.Core.TyCon where

import Unbound.LocallyNameless

import {-# SOURCE #-} CLaSH.Core.Term
import {-# SOURCE #-} CLaSH.Core.Type

data TyCon
data PrimRep
  = AddrRep
  | IntRep

type TyConName = Name TyCon

instance Rep   TyCon
instance Show  TyCon
instance Alpha TyCon
instance Subst Type TyCon
instance Subst Term TyCon

pcPrimTyCon0 ::
  TyConName
  -> PrimRep
  -> TyCon
