{-|
Copyright  :  (C) 2017-2018, Google Inc
                  2019     , Myrtle Software
                  2022     , QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

PLL and other clock-related components for Intel (Altera) FPGAs

A PLL generates a stable clock signal for your circuit at a selectable
frequency.

If you haven't determined the frequency you want the circuit to run at, the
predefined 100 MHz domain `Clash.Signal.System` can be a good starting point.
The datasheet for your FPGA specifies lower and upper limits, but the true
maximum frequency is determined by your circuit. The 'altpll' and 'alteraPll'
components below show an example for when the oscillator connected to the FPGA
runs at 50 MHz. If the oscillator runs at 100 MHz, change @DomInput@ to:

@
'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"DomInput\", vPeriod=10000}
@

We suggest you always use a PLL even if your oscillator runs at the frequency
you want to run your circuit at.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.Intel.ClockGen
  ( altpllSync
  , alteraPllSync
  , unsafeAltpll
  , unsafeAlteraPll
    -- ** Deprecated
  , altpll
  , alteraPll
  ) where

import GHC.TypeLits (type (<=))

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Clocks
  (Clocks(..), ClocksSync(..), ClocksSyncCxt, NumOutClocksSync)
import Clash.Magic (setName)
import Clash.Promoted.Symbol (SSymbol)
import Clash.Signal.Internal
  (Signal, Clock, Reset, KnownDomain, HasAsynchronousReset)

altpllSync ::
  forall t domIn .
  ( HasAsynchronousReset domIn
  , ClocksSyncCxt t domIn
  , NumOutClocksSync t domIn <= 5
  ) =>
  Clock domIn ->
  Reset domIn ->
  t
altpllSync clkIn rstIn =
  clocksResetSynchronizer (unsafeAltpll clkIn rstIn) clkIn

-- | A clock source that corresponds to the Intel/Quartus \"ALTPLL\" component
-- (Arria GX, Arria II, Stratix IV, Stratix III, Stratix II, Stratix,
--  Cyclone 10 LP, Cyclone IV, Cyclone III, Cyclone II, Cyclone)
-- with settings to provide a stable 'Clock' from a single free-running input
--
-- Only works when configured with:
--
-- * 1 reference clock
-- * 1 output clock
-- * a reset input port
-- * a locked output port
--
-- The PLL lock output is asserted when the clock is stable, and is usually
-- connected to reset circuitry to keep the circuit in reset while the clock is
-- still stabilizing.
--
-- See also the [ALTPLL (Phase-Locked Loop) IP Core User Guide](https://www.intel.com/content/dam/www/programmable/us/en/pdfs/literature/ug/ug_altpll.pdf)
--
-- === Example
--
-- ==== Using a PLL
--
-- When the oscillator connected to the FPGA runs at 50 MHz and the external
-- reset signal is /active low/, this will generate a 100 MHz clock for the
-- @'Clash.Signal.System'@ domain:
--
-- @
-- 'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"DomInput\", vPeriod=20000}
--
-- topEntity
--   :: 'Clock' DomInput
--   -> 'Signal' DomInput 'Bool'
--   -> [...]
-- topEntity clkInp rstInp = [...]
--  where
--   (clk, pllStable) =
--     'altpll' \@'Clash.Signal.System' ('SSymbol' \@\"altpll50to100\") clkInp
--            ('Clash.Signal.unsafeFromActiveLow' rstInp)
--   rst = 'Clash.Signal.resetSynchronizer' clk ('Clash.Signal.unsafeFromActiveLow' pllStable)
-- @
--
-- 'Clash.Signal.resetSynchronizer' will keep the reset asserted when
-- @pllStable@ is 'False', hence the use of
-- @'Clash.Signal.unsafeFromActiveLow' pllStable@. Your circuit will have
-- signals of type @'Signal' 'Clash.Signal.System'@ and all the clocks and
-- resets of your components will be the @clk@ and @rst@ signals generated here
-- (modulo local resets, which will be based on @rst@ or never asserted at all
-- if the component doesn't need a reset).
--
-- ==== Specifying the output frequency
--
-- If you don't have a top-level type signature specifying the output clock
-- domain, you can use type applications to specify it, e.g.:
--
-- @
-- 'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"Dom100MHz\", vPeriod=10000}
--
-- -- outputs a clock running at 100 MHz
-- (clk100, pllLocked) = 'altpll' \@Dom100MHz ('SSymbol' \@\"altpll50to100\") clk50 rst50
-- @
altpll ::
  forall domOut domIn name .
  ( HasAsynchronousReset domIn
  , KnownDomain domOut
  ) =>
  -- | Name of the component instance
  --
  -- Instantiate as follows: @(SSymbol \@\"altpll50\")@
  SSymbol name ->
  -- | Free running clock (e.g. a clock pin connected to a crystal oscillator)
  Clock domIn ->
  -- | Reset for the PLL
  Reset domIn ->
  -- | (Stable PLL clock, PLL lock)
  (Clock domOut, Signal domOut Bool)
altpll _ = setName @name unsafeAltpll
{-# INLINE altpll #-}
{-# DEPRECATED altpll "This function is unsafe. Please see documentation of the function for alternatives." #-}

unsafeAltpll ::
  forall t domIn .
  ( KnownDomain domIn
  , Clocks t
  , ClocksCxt t
  , NumOutClocks t <= 5
  ) =>
  Clock domIn ->
  Reset domIn ->
  t
unsafeAltpll = clocks
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unsafeAltpll #-}
{-# ANN unsafeAltpll hasBlackBox #-}

alteraPllSync ::
  forall t domIn .
  ( HasAsynchronousReset domIn
  , ClocksSyncCxt t domIn
  , NumOutClocksSync t domIn <= 18
  ) =>
  Clock domIn ->
  Reset domIn ->
  t
alteraPllSync clkIn rstIn =
  clocksResetSynchronizer (unsafeAlteraPll clkIn rstIn) clkIn

-- | A clock source that corresponds to the Intel/Quartus \"Altera PLL\"
-- component (Arria V, Stratix V, Cyclone V) with settings to provide a stable
-- 'Clock' from a single free-running input
--
-- Only works when configured with:
--
-- * 1 reference clock
-- * 1-16 output clocks
-- * a reset input port
-- * a locked output port
--
-- The PLL lock output is asserted when the clocks are stable, and is usually
-- connected to reset circuitry to keep the circuit in reset while the clocks
-- are still stabilizing.
--
-- See also the [Altera Phase-Locked Loop (Altera PLL) IP Core User Guide](https://www.intel.com/content/dam/www/programmable/us/en/pdfs/literature/ug/altera_pll.pdf)
--
-- === Specifying outputs
--
-- The number of output clocks depends on this function's inferred result type.
-- An instance with a single output clock can be instantiated using:
--
-- @
-- 'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"Dom100MHz\", vPeriod=10000}
--
-- (clk100 :: 'Clock' Dom100MHz, pllLocked) =
--   'alteraPll' ('SSymbol' \@\"alterapll50to100\") clk50 rst50
-- @
--
-- An instance with two clocks can be instantiated using
--
-- @
-- 'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"Dom100MHz\", vPeriod=10000}
-- 'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"Dom150MHz\", vPeriod='Clash.Signal.hzToPeriod' 150e6}
--
-- (clk100 :: 'Clock' Dom100MHz, clk150 :: 'Clock' Dom150MHz, pllLocked) =
--   'alteraPll' ('SSymbol' \@\"alterapllmulti\") clk50 rst50
-- @
--
-- and so on up to 16 clocks.
--
-- If you don't have a top-level type signature specifying the output clock
-- domains, you can specify them using a pattern type signature, as shown here.
--
-- === Example
--
-- When the oscillator connected to the FPGA runs at 50 MHz and the external
-- reset signal is /active low/, this will generate a 100 MHz clock for the
-- @'Clash.Signal.System'@ domain:
--
-- @
-- 'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"DomInput\", vPeriod=20000}
--
-- topEntity
--   :: 'Clock' DomInput
--   -> 'Signal' DomInput 'Bool'
--   -> [...]
-- topEntity clkInp rstInp = [...]
--  where
--   (clk :: 'Clock' 'Clash.Signal.System', pllStable :: 'Signal' 'Clash.Signal.System' 'Bool')
--     'alteraPll' ('SSymbol' \@\"alterapll50to100\") clkInp
--               ('Clash.Signal.unsafeFromActiveLow' rstInp)
--   rst = 'Clash.Signal.resetSynchronizer' clk ('Clash.Signal.unsafeFromActiveLow' pllStable)
-- @
--
-- 'Clash.Signal.resetSynchronizer' will keep the reset asserted when
-- @pllStable@ is 'False', hence the use of
-- @'Clash.Signal.unsafeFromActiveLow' pllStable@. Your circuit will have
-- signals of type @'Signal' 'Clash.Signal.System'@ and all the clocks and
-- resets of your components will be the @clk@ and @rst@ signals generated here
-- (modulo local resets, which will be based on @rst@ or never asserted at all
-- if the component doesn't need a reset).
alteraPll ::
  forall t domIn name .
  ( HasAsynchronousReset domIn
  , Clocks t
  , ClocksCxt t
  , NumOutClocks t <= 18
  ) =>
  -- | Name of the component instance
  --
  -- Instantiate as follows: @(SSymbol \@\"alterapll50\")@
  SSymbol name ->
  -- | Free running clock (e.g. a clock pin connected to a crystal oscillator)
  Clock domIn ->
  -- | Reset for the PLL
  Reset domIn ->
  t
alteraPll _ = setName @name unsafeAlteraPll
{-# INLINE alteraPll #-}
{-# DEPRECATED alteraPll "This function is unsafe. Please see documentation of the function for alternatives." #-}

unsafeAlteraPll ::
  forall t domIn .
  ( KnownDomain domIn
  , Clocks t
  , ClocksCxt t
  , NumOutClocks t <= 18
  ) =>
  Clock domIn ->
  Reset domIn ->
  t
unsafeAlteraPll = clocks
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unsafeAlteraPll #-}
{-# ANN unsafeAlteraPll hasBlackBox #-}
