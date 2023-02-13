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
import Clash.Clocks (clocks)
import Clash.Promoted.Symbol (SSymbol)
import Clash.Signal.Internal

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
clockWizard
  :: forall domIn domOut pllLock name
   . ( KnownDomain domIn
     , KnownDomain domOut
     , KnownDomain pllLock)
  => SSymbol name
  -- ^ Name of the component instance
  --
  -- Instantiate as follows: @(SSymbol \@\"clockWizard50\")@
  -> Clock domIn
  -- ^ Free running clock (i.e. a clock pin connected to a crystal)
  -> Reset domIn
  -- ^ Reset for the PLL
  -> (Clock domOut, Signal pllLock Bool)
  -- ^ (Stable PLL clock, PLL lock)
clockWizard !_ = clocks
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE clockWizard #-}
{-# ANN clockWizard hasBlackBox #-}

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
clockWizardDifferential
  :: forall domIn domOut pllLock name
   . ( KnownDomain domIn
     , KnownDomain domOut
     , KnownDomain pllLock)
  => SSymbol name
  -- ^ Name of the component instance
  --
  -- Instantiate as follows: @(SSymbol \@\"clockWizardD50\")@
  -> DiffClock domIn
  -- ^ Free running clock
  -> Reset domIn
  -- ^ Reset for the PLL
  -> (Clock domOut, Signal pllLock Bool)
  -- ^ (Stable PLL clock, PLL lock)
clockWizardDifferential !_ (DiffClock clk _) = clocks clk
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE clockWizardDifferential #-}
{-# ANN clockWizardDifferential hasBlackBox #-}
