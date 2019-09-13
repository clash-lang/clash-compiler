{-|
  Copyright   :  (C) 2019, Google Inc
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

module Clash.Netlist.BlackBox where

import Data.Text (Text)
import Clash.Core.Term (Term)
import Clash.Core.Var (Id)
import Clash.Netlist.Types (BlackBoxContext, Declaration, NetlistMonad)
import Clash.Primitives.Types (CompiledPrimitive)

extractPrimWarnOrFail
  :: Text
  -> NetlistMonad CompiledPrimitive

mkBlackBoxContext
  :: Text
  -- ^ Blackbox function name
  -> Id
  -- ^ Identifier binding the primitive/blackbox application
  -> [Term]
  -- ^ Arguments of the primitive/blackbox application
  -> NetlistMonad (BlackBoxContext,[Declaration])
