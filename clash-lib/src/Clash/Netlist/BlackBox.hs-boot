{-|
  Copyright   :  (C) 2019, Google Inc
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
module Clash.Netlist.BlackBox where

import Clash.Core.Term (Term)
import Clash.Core.Type (Type)
import Clash.Core.Var (Id)
import Clash.Netlist.Types (BlackBoxContext, Declaration, DeclarationType, NetlistMonad)
import Clash.Primitives.Types (CompiledPrimitive)
import Data.Text (Text)
import GHC.Stack (HasCallStack)

extractPrimWarnOrFail ::
  (HasCallStack) =>
  String ->
  Text ->
  NetlistMonad CompiledPrimitive
mkBlackBoxContext ::
  (HasCallStack) =>
  -- | Blackbox function name
  Text ->
  -- | Are we concurrent or sequential?
  DeclarationType ->
  -- | Identifiers binding the primitive/blackbox application
  [Id] ->
  -- | Arguments of the primitive/blackbox application
  [Either Term Type] ->
  NetlistMonad (BlackBoxContext, [Declaration])
