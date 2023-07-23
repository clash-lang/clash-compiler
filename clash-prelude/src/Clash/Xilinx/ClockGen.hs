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

module Clash.Xilinx.ClockGen where

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Clocks (clocks, clocksResetSynchronizer)
import Clash.Promoted.Symbol (SSymbol)
import Clash.Signal.Internal

clockWizardSync ::
  forall domIn domOut name .
  ( KnownDomain domIn
  , KnownDomain domOut
  ) =>
  SSymbol name ->
  Clock domIn ->
  Reset domIn ->
  (Clock domOut, Reset domOut)
clockWizardSync name clkIn rstIn = clocksResetSynchronizer (unsafeClockWizard name clkIn rstIn) clkIn

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
clockWizard ::
  forall domIn domOut pllLock name .
  ( KnownDomain domIn
  , KnownDomain domOut
  , KnownDomain pllLock
  ) =>
  -- | Name of the component instance
  --
  -- Instantiate as follows: @(SSymbol \@\"clockWizard50\")@
  SSymbol name ->
  -- | Free running clock (i.e. a clock pin connected to a crystal)
  Clock domIn ->
  -- | Reset for the PLL
  Reset domIn ->
  -- | (Stable PLL clock, PLL lock)
  (Clock domOut, Signal pllLock Bool)
clockWizard = unsafeClockWizard

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
  forall domIn domOut pllLock name .
  ( KnownDomain domIn
  , KnownDomain domOut
  , KnownDomain pllLock
  ) =>
  -- | Name of the component instance
  --
  -- Instantiate as follows: @(SSymbol \@\"clockWizard50\")@
  SSymbol name ->
  -- | Free running clock (i.e. a clock pin connected to a crystal)
  Clock domIn ->
  -- | Reset for the PLL
  Reset domIn ->
  -- | (Stable PLL clock, PLL lock)
  (Clock domOut, Signal pllLock Bool)
unsafeClockWizard !_ = clocks
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unsafeClockWizard #-}
{-# ANN unsafeClockWizard hasBlackBox #-}

clockWizardDifferentialSync ::
  forall domIn domOut name .
  ( KnownDomain domIn
  , KnownDomain domOut
  ) =>
  SSymbol name ->
  DiffClock domIn ->
  Reset domIn ->
  (Clock domOut, Reset domOut)
clockWizardDifferentialSync name clkIn@(DiffClock clkInP _) rstIn =
  clocksResetSynchronizer
    (unsafeClockWizardDifferential name clkIn rstIn)
    clkInP

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
clockWizardDifferential ::
  forall domIn domOut pllLock name .
  ( KnownDomain domIn
  , KnownDomain domOut
  , KnownDomain pllLock
  ) =>
  -- | Name of the component instance
  --
  -- Instantiate as follows: @(SSymbol \@\"clockWizardD50\")@
  SSymbol name ->
  -- | Free running clock
  DiffClock domIn ->
  -- | Reset for the PLL
  Reset domIn ->
  -- | (Stable PLL clock, PLL lock)
  (Clock domOut, Signal pllLock Bool)
clockWizardDifferential = unsafeClockWizardDifferential

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
  forall domIn domOut pllLock name .
  ( KnownDomain domIn
  , KnownDomain domOut
  , KnownDomain pllLock
  ) =>
  -- | Name of the component instance
  --
  -- Instantiate as follows: @(SSymbol \@\"clockWizardD50\")@
  SSymbol name ->
  -- | Free running clock
  DiffClock domIn ->
  -- | Reset for the PLL
  Reset domIn ->
  -- | (Stable PLL clock, PLL lock)
  (Clock domOut, Signal pllLock Bool)
unsafeClockWizardDifferential !_ (DiffClock clk _) = clocks clk
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unsafeClockWizardDifferential #-}
{-# ANN unsafeClockWizardDifferential hasBlackBox #-}
