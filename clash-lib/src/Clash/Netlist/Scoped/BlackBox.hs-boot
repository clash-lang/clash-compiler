{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  For "Clash.Netlist.Util", which "Clash.Netlist.Scoped.BlackBox"
  imports.
-}

module Clash.Netlist.Scoped.BlackBox
  ( mkBlackBoxContext
  , extractPrimWarnOrFail
  ) where

import Data.Text (Text)
import GHC.Stack (HasCallStack)

import Clash.Core.Scoped.Syntax (Distinct, Term, Type)
import Clash.Netlist.Types (CompCtx, LocalId)
import Clash.Netlist.Types (BlackBoxContext, Declaration, DeclarationType, NetlistMonad)
import Clash.Primitives.Types (CompiledPrimitive)

mkBlackBoxContext
  :: (HasCallStack, Distinct n)
  => CompCtx n
  -> Text
  -> DeclarationType
  -> [LocalId n]
  -> [Either (Term n) (Type n)]
  -> NetlistMonad (BlackBoxContext, [Declaration])

extractPrimWarnOrFail
  :: HasCallStack
  => String
  -> Text
  -> NetlistMonad CompiledPrimitive
