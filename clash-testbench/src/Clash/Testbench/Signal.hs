{-|
Copyright:    (C) 2023 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>

'Clash.Testbench.Simulate.TB' lifted signals (internal).
-}
module Clash.Testbench.Signal
  ( TBSignal
  , TBClock
  , TBReset
  , TBEnable
  , AutoTB(..)
  ) where

import Clash.Prelude (KnownDomain)

import qualified Clash.Testbench.Internal.Signal as Internal

-- | A 'Clash.Signal.Signal' that has been lifted into the
-- 'Clash.Testbench.Simulate.TB' context.
type TBSignal dom = Internal.TBSignal 'Internal.USER dom

-- | A 'Clash.Signal.Clock' signal that has been lifted into the
-- 'Clash.Testbench.Simulate.TB' context.
type TBClock dom  = Internal.TBClock 'Internal.USER dom

-- | A 'Clash.Signal.Reset' signal that has been lifted into the
-- 'Clash.Testbench.Simulate.TB' context.
type TBReset dom  = Internal.TBReset 'Internal.USER dom

-- | An 'Clash.Signal.Enable' signal that has been lifted into the
-- 'Clash.Testbench.Simulate.TB' context.
type TBEnable dom = Internal.TBEnable 'Internal.USER dom

-- | Signals that are implicitly available inside
-- 'Clash.Testbench.Simulate.TB' and can be driven by the simulator
-- automatically.
class AutoTB a where
  auto :: a

instance KnownDomain dom => AutoTB (TBClock dom) where
  auto = Internal.AutoClock

instance KnownDomain dom => AutoTB (TBReset dom) where
  auto = Internal.AutoReset

instance KnownDomain dom => AutoTB (TBEnable dom) where
  auto = Internal.AutoEnable
