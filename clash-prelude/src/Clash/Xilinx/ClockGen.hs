{-|
Copyright  :  (C) 2017, Google Inc,
                  2023, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

PLL and other clock-related components for Xilinx FPGAs
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Clash.Xilinx.ClockGen
  ( clockWizard
  , clockWizardDifferential
  , unsafeClockWizard
  , unsafeClockWizardDifferential
  ) where

import GHC.TypeLits (type (<=))

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Clocks
  (Clocks(..), ClocksSync(..), ClocksSyncCxt, NumOutClocksSync)
import Clash.Signal.Internal
  (Clock, DiffClock(..), Reset, KnownDomain, HasAsynchronousReset)

clockWizard ::
  forall t domIn .
  ( HasAsynchronousReset domIn
  , ClocksSyncCxt t domIn
  , NumOutClocksSync t domIn <= 7
  ) =>
  Clock domIn ->
  Reset domIn ->
  t
clockWizard clkIn rstIn =
  clocksResetSynchronizer (unsafeClockWizard clkIn rstIn) clkIn

-- | A clock source that corresponds to the Xilinx MMCM component created
-- with the \"Clock Wizard\" with settings to provide a stable 'Clock' from
-- a single free-running clock input.
--
-- You can use type applications to specify the output clock domain, e.g.:
--
-- @
-- createDomain vXilinxSystem{vName=\"Dom100MHz\", vPeriod=10000}
--
-- -- Outputs a clock running at 100 MHz
-- clockWizard \@_ \@Dom100MHz (SSymbol \@\"clkWizard50to100\") clk50 rst
-- @
--
-- See also the [Clocking Wizard LogiCORE IP Product Guide](https://docs.xilinx.com/r/en-US/pg065-clk-wiz)
unsafeClockWizard ::
  forall t domIn .
  ( KnownDomain domIn
  , Clocks t
  , ClocksCxt t
  , NumOutClocks t <= 7
  ) =>
  -- | Free running clock (i.e. a clock pin connected to a crystal)
  Clock domIn ->
  -- | Reset for the PLL
  Reset domIn ->
  t
unsafeClockWizard = clocks
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unsafeClockWizard #-}
{-# ANN unsafeClockWizard hasBlackBox #-}

clockWizardDifferential ::
  forall t domIn .
  ( HasAsynchronousReset domIn
  , ClocksSyncCxt t domIn
  , NumOutClocksSync t domIn <= 7
  ) =>
  DiffClock domIn ->
  Reset domIn ->
  t
clockWizardDifferential clkIn@(DiffClock clkInP _) rstIn =
  clocksResetSynchronizer (unsafeClockWizardDifferential clkIn rstIn) clkInP

-- | A clock source that corresponds to the Xilinx MMCM component created
-- with the \"Clock Wizard\", with settings to provide a stable 'Clock'
-- from a free-running differential clock input.
--
-- You can use type applications to specify the output clock domain, e.g.:
--
-- @
-- createDomain vXilinxSystem{vName=\"Dom100MHz\", vPeriod=10000}
--
-- -- Outputs a clock running at 100 MHz
-- clockWizardDifferential \@_ \@Dom100MHz (SSymbol \@\"clkWizard50to100\") clk50 rst
-- @
--
-- See also the [Clocking Wizard LogiCORE IP Product Guide](https://docs.xilinx.com/r/en-US/pg065-clk-wiz)
unsafeClockWizardDifferential ::
  forall t domIn .
  ( KnownDomain domIn
  , Clocks t
  , ClocksCxt t
  , NumOutClocks t <= 7
  ) =>
  -- | Free running clock
  DiffClock domIn ->
  -- | Reset for the PLL
  Reset domIn ->
  t
unsafeClockWizardDifferential (DiffClock clk _) = clocks clk
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unsafeClockWizardDifferential #-}
{-# ANN unsafeClockWizardDifferential hasBlackBox #-}
