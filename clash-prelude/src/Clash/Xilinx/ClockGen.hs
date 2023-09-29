{-|
Copyright  :  (C) 2017, Google Inc,
                  2023, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

This module contains functions for instantiating clocking circuits on Xilinx
FPGA's.

We suggest you always use a clocking circuit even if your oscillator runs at the
frequency you want to run your circuit at.

A clocking circuit generates a stable clock signal for your design at a
configurable frequency. A clocking circuit in an FPGA is frequently referred to
as a PLL (Phase-Locked Loop). However, Xilinx differentiates between several
types of clocking circuit implementations in their FPGAs and uses the term PLL
to refer to one specific type, so we choose to use the more generic term
/clocking circuit/ here as well.

For most use cases, you would create two or more synthesis domains describing
the oscillator input and the domains you wish to use in your design, and use the
[regular functions](#g:regular) below to generate the clocks and resets of the
design from the oscillator input. There are use cases not covered by this
simpler approach, and the [unsafe functions](#g:unsafe) are provided as a means
to build advanced reset managers for the output domains.

Synthesis domains are denoted by the type-parameter
@dom :: t'Clash.Signal.Domain'@ as occuring in for instance
@t'Clash.Signal.Signal' dom a@; see "Clash.Signal" for more information. There
is a one-to-one correspondence between clock signals and synthesis domains: only
a single clock signal will provide the clock for all elements in the synthesis
domain.

For the input domain, usually only two parameters are relevant: the frequency or
period, and the reset polarity. If the clock input is a free-running clock at a
frequency of 50 MHz (a period of 20 ns or 20,000 ps), and the reset input
connected to the clocking circuit is active-low, the following will instantiate
the required input domain:

@
'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"DomInput\", vPeriod=20000, vResetPolarity='Clash.Signal.ActiveLow'}
@

If you haven't determined the frequency you want the design to run at, the
predefined 100 MHz domain t'Clash.Signal.XilinxSystem' can be a good starting
point. The datasheet for your FPGA specifies lower and upper limits, but the
true maximum frequency is determined by your design.

However, supposing you need a clock running at 150 MHz for your design, the
following will instantiate a suitable domain:

@
'Clash.Signal.createDomain' 'Clash.Signal.vXilinxSystem'{vName=\"Dom150\", vPeriod='Clash.Signal.hzToPeriod' 150e6}
@

@Dom150@ will have 'Clash.Signal.Synchronous' resets on its memory elements
because it was derived from 'Clash.Signal.vXilinxSystem', whereas @DomInput@
will have asynchronous resets because it was derived from
'Clash.Signal.vSystem'. Xilinx recommends synchronous resets for circuits, but
the clocking circuit reacts asynchronously to its reset input instead, which
will need to be declared correctly in Clash. If you use the /unsafe/ functions
below, Clash does not enforce this.

The functions in this module will instantiate a Xilinx MMCM clocking circuit
corresponding to the Xilinx \"Clock Wizard\" with 1 reference clock input and a
reset input, and 1 to 7 output clocks and a @locked@ output.

The functions in [Regular functions](#g:regular) incorporate
'Clash.Signal.resetSynchronizer' to convert the @locked@ output port into a
proper 'Reset' signal for the domains which will keep the circuit in reset while
the clock is still stabilizing.

The clocking circuit will react asynchronously to the incoming reset input. When
the reset input is asserted, the clocking circuit's @locked@ output will
deassert, in turn causing the 'Reset' output(s) of these functions to assert.

You can use 'Clash.Magic.setName' to give the IP instance a specific name, which
can be useful if you need to refer to the instance in Synopsys Design
Constraints files.

If you need access to the @locked@ output to build a more advanced reset
manager, you should use the [unsafe functions](#g:unsafe) instead.

See also the [Clocking Wizard LogiCORE IP Product Guide](https://docs.xilinx.com/r/en-US/pg065-clk-wiz)

== Example

=== Using a PLL

The clock domain connected to the /input/ of the PLL will need to declare it has
/asynchronous/ resets, as the PLL reacts asynchronously to the reset signal.
Conversely, Xilinx recommends you use /synchronous/ resets for your circuit, so
you will usually use a domain with synchronous resets on the /output/ of the
PLL.

The t'Clash.Signal.System' domain has /asynchronous/ resets, and the
t'Clash.Signal.XilinxSystem' domain has /synchronous/ resets. So these form a
good basis to derive your custom clock domains.

When the oscillator connected to the FPGA runs at 50 MHz and the external reset
signal is /active low/, this will generate a 150 MHz clock for use by the
circuit:

@
'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"DomInput\", vPeriod=20000, vResetPolarity=ActiveLow}
'Clash.Signal.createDomain' 'Clash.Signal.vXilinxSystem'{vName=\"Dom150\", vPeriod='Clash.Signal.hzToPeriod' 150e6}

topEntity
  :: 'Clock' DomInput
  -> 'Reset' DomInput
  -> [...]
topEntity clkInp rstInp = [...]
 where
  (clk, rst) = altpllSync \@System clkInp rstInp
@

Your circuit will have signals of type @'Signal' t'Clash.Signal.System'@ and
all the clocks and resets of your components will be the @clk@ and @rst@
signals generated here (modulo local resets, which will be based on @rst@ or
never asserted at all if the component doesn't need a reset).

=== Specifying the output frequency

If you don't have a top-level type signature specifying the output clock
domain, you can use type applications to specify it, e.g.:

@
'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"Dom100MHz\", vPeriod=10000}

-- outputs a clock running at 100 MHz
(clk100, rst100) = altpllSync \@Dom100MHz clk50 rst50
@

-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Clash.Xilinx.ClockGen
  ( -- * Tcl
    -- $tcl

    -- * Regular functions #regular#
    -- $regular
    clockWizard
  , clockWizardDifferential
    -- * Unsafe functions #unsafe#
    -- $unsafe
  , unsafeClockWizard
  , unsafeClockWizardDifferential
  ) where

import GHC.TypeLits (type (<=))

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Clocks
  (Clocks(..), ClocksSync(..), ClocksSyncCxt, NumOutClocksSync)
import Clash.Signal.Internal
  (Clock, DiffClock(..), Reset, KnownDomain, HasAsynchronousReset)

{- $tcl
When generating HDL, these functions will emit a Tcl script for Vivado that
instantiates the needed IP core for the function. This Tcl script adheres to the
Clash\<-\>Tcl API. The Tcl Connector bundled in @clash-lib:Clash.DataFiles@ will
automatically process these scripts and build your project in Vivado. See
@clash-lib:Clash.DataFiles@ for more information.
-}

{- $regular
-}

{- $unsafe
Let's discuss the unsafe functions
-}

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
