module Clash.Core.Subst where

import GHC.Stack (HasCallStack)
import {-# SOURCE #-} Clash.Core.Type (Type)
import Clash.Core.Var (TyVar)

substTyWith
  :: HasCallStack
  => [TyVar]
  -> [Type]
  -> Type
  -> Type

aeqType
  :: Type
  -> Type
  -> Bool
