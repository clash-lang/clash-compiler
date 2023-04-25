{-|
Copyright:    (C) 2023 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>

'Clash.Testbench.Simulate.TB' lifted signals.
-}

module Clash.Testbench.Signal
  ( TBSignal
  , TBClock
  , TBReset
  , TBEnable
  ) where

import Clash.Testbench.Internal.ID (Stage(..))
import qualified Clash.Testbench.Internal.Signal as Internal

-- | A 'Clash.Signal.Signal' that has been lifted into the 'Clash.Testbench.Simulate.TB' context.
type TBSignal dom = Internal.TBSignal 'USER dom

-- | A 'Clash.Signal.Clock' signal that has been lifted into the 'Clash.Testbench.Simulate.TB' context.
type TBClock dom  = Internal.TBClock 'USER dom

-- | A 'Clash.Signal.Reset' signal that has been lifted into the 'Clash.Testbench.Simulate.TB' context.
type TBReset dom  = Internal.TBReset 'USER dom

-- | An 'Clash.Signal.Enable' signal that has been lifted into the 'Clash.Testbench.Simulate.TB' context.
type TBEnable dom = Internal.TBEnable 'USER dom
