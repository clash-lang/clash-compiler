{-# OPTIONS_GHC -fno-warn-orphans #-}

module Clash.Core.Subst where

import {-# SOURCE #-} Clash.Core.Term (Term, TickInfo)
import {-# SOURCE #-} Clash.Core.Type (Type)
import Clash.Core.Var (TyVar)
import GHC.Stack (HasCallStack)

substTyWith ::
  (HasCallStack) =>
  [TyVar] ->
  [Type] ->
  Type ->
  Type
aeqType ::
  Type ->
  Type ->
  Bool

instance Eq Type

instance Ord Type

acmpTickInfo :: TickInfo -> TickInfo -> Ordering

instance Eq Term
