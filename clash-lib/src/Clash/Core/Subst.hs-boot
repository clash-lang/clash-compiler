module Clash.Core.Subst where

import {-# SOURCE #-} Clash.Core.Type (Type)
import Clash.Core.Var (TyVar)

substTyWith
  :: [TyVar]
  -> [Type]
  -> Type
  -> Type

aeqType
  :: Type
  -> Type
  -> Bool
