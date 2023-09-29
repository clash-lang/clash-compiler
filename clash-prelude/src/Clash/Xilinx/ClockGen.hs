{-|
Copyright  :  (C) 2017, Google Inc,
                  2023, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

This module contains functions for instantiating clock generators on Xilinx
FPGA's.

We suggest you use a clock generator even if your oscillator runs at the
frequency you want to run your circuit at.

A clock generator generates a stable clock signal for your design at a
configurable frequency. A clock generator in an FPGA is frequently referred to
as a PLL (Phase-Locked Loop). However, Xilinx differentiates between several
types of clock generator implementations in their FPGAs and uses the term PLL to
refer to one specific type, so we choose to use the more generic term /clock/
/generator/ here.

For most use cases, you would create two or more synthesis domains describing
the oscillator input and the domains you wish to use in your design, and use
the [regular functions](#g:regular) below to generate the clocks and resets of
the design from the oscillator input. There are use cases not covered by this
simpler approach, and the [unsafe functions](#g:unsafe) are provided as a means
to build advanced reset managers for the output domains.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Clash.Xilinx.ClockGen
  ( -- * Choosing domains
    -- $domains

    -- ** Caution: actual output frequency
    -- $caution

    -- * Using
    -- $using

    -- ** Example
    -- $example

    -- ** Type checking errors
    -- $error

    -- ** Tcl
    -- $tcl

    -- * Regular functions #regular#
    clockWizard
  , clockWizardDifferential
    -- * Unsafe functions #unsafe#
    -- $unsafe

    -- ** Example
    -- $unsafe_example
  , unsafeClockWizard
  , unsafeClockWizardDifferential
  ) where

import GHC.TypeLits (type (<=))

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Clocks
  (Clocks(..), ClocksSync(..), ClocksSyncCxt, NumOutClocksSync)
import Clash.Signal.Internal
  (Clock, DiffClock(..), Reset, KnownDomain, HasAsynchronousReset)

{- $domains
Synthesis domains are denoted by the type-parameter
@dom :: t'Clash.Signal.Domain'@ as occurring in for instance
@t'Clash.Signal.Signal' dom a@; see "Clash.Signal" for more information. For
each domain, there is only a single clock signal which clocks that domain;
mixing clock signals is a design error. Conversely, it is possible to clock
multiple domains using the same clock signal, in complex designs.

For the clock generator inputs, create a domain with the correct clock frequency
and reset polarity. For instance, if the clock input is a free-running clock at
a frequency of 50 MHz (a period of 20 ns or 20,000 ps), and the reset input
connected to the clock generator is /active-low/, the following will instantiate
the required input domain:

@
'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"DomInput\", vPeriod=20000, vResetPolarity='Clash.Signal.ActiveLow'}
@

If you haven't determined the frequency you want the design to run at, the
predefined 100 MHz domain t'Clash.Signal.XilinxSystem' can be a good starting
point. The datasheet for your FPGA specifies lower and upper limits, but the
true maximum frequency is determined by your design.

Supposing you need a clock running at 150 MHz for your design, the following
will instantiate a suitable domain:

@
'Clash.Signal.createDomain' 'Clash.Signal.vXilinxSystem'{vName=\"Dom150\", vPeriod='Clash.Signal.hzToPeriod' 150e6}
@

@Dom150@ will have 'Clash.Signal.Synchronous' resets on its memory elements
because it was derived from 'Clash.Signal.vXilinxSystem', whereas @DomInput@
will have 'Clash.Signal.Asynchronous' resets because it was derived from
'Clash.Signal.vSystem'. Xilinx recommends synchronous resets for circuits, but
the clock generator reacts asynchronously to its reset input instead, which will
need to be declared correctly in Clash. If you use the /unsafe/ functions below,
Clash does not enforce this.
-}

{- $caution
The clock generator in the FPGA is limited in which clock frequencies it can
generate, especially when one clock generator has multiple outputs. The clock
generator will pick the attainable frequency closest to the requested frequency
(or possibly fail to synthesize). You can check the frequency that the wizard
chose by loading your design into the Vivado GUI. In the /IP sources/ window,
choose the clock wizard and select /Re-customize IP.../. On the /Output Clocks/
tab, the relevant column is /Actual Output Freq (MHz)/. If the actual value
differs, copy the actual value back to the Clash design.
-}

{- $using
The functions in this module will instantiate a Xilinx MMCM clock generator
corresponding to the Xilinx \"Clock Wizard\" with 1 reference clock input and a
reset input, and 1 to 7 output clocks and a @locked@ output.

The [regular functions](#g:regular) incorporate 'Clash.Signal.resetSynchronizer'
to convert the @locked@ output port into a proper 'Reset' signal for the domains
which will keep the circuit in reset while the clock is still stabilizing.

The clock generator will react asynchronously to the incoming reset input. When
the reset input is asserted, the clock generator's @locked@ output will
deassert, in turn causing the 'Reset' output(s) of these functions to assert.

You can use 'Clash.Magic.setName' to give the IP instance a specific name, which
can be useful if you need to refer to the instance in Synopsys Design
Constraints files.

The output of the function for /n/ output clocks is a /2n/-tuple with clock and
reset outputs. The compiler needs to be able to fully determine the types of the
individual tuple elements from the context; the clock generator function itself
will not constrain them. If the types of the tuple elements cannot be inferred,
you can use pattern type signatures to specify the types. Supposing the
referenced domains have been created with 'Clash.Signal.createDomain', an
instance with a single output clock can be instantiated using:

@
(clk150 :: 'Clock' Dom150, rst150 :: 'Reset' Dom150) = 'clockWizard' clkIn rstIn
@

An instance with two clocks can be instantiated using

@
( clk100 :: 'Clock' Dom100
  , rst100 :: 'Reset' Dom100
  , clk150 :: 'Clock' Dom150
  , rst150 :: 'Reset' Dom150) = 'clockWizard' clkIn rstIn
@

and so on up to 7 clocks, following the general pattern @('Clock' dom1, 'Reset'
dom1, 'Clock' dom2, 'Reset' dom2, ..., 'Clock' dom/n/, 'Reset' dom/n/)@.

If you need access to the @locked@ output to build a more advanced reset
manager, you should use the [unsafe functions](#g:unsafe) instead.

See also the [Clocking Wizard LogiCORE IP Product Guide](https://docs.xilinx.com/r/en-US/pg065-clk-wiz)
-}

{- $example

When the oscillator connected to the FPGA runs at 50 MHz and the external reset
signal is /active-low/, this will generate a 150 MHz clock for use by the
circuit:

@
'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"DomInput\", vPeriod=20000, vResetPolarity='Clash.Signal.ActiveLow'}
'Clash.Signal.createDomain' 'Clash.Signal.vXilinxSystem'{vName=\"Dom150\", vPeriod='Clash.Signal.hzToPeriod' 150e6}

topEntity
  :: 'Clock' DomInput
  -> 'Reset' DomInput
  -> t'Clash.Signal.Signal' Dom150 Int
  -> t'Clash.Signal.Signal' Dom150 Int
topEntity clkIn rstIn = 'Clash.Signal.exposeClockResetEnable' (register 0) clk rst 'Clash.Signal.enableGen'
 where
  (clk, rst) = 'clockWizard' clkIn rstIn
@
-}

{- $error
When type checking cannot infer the types of the tuple elements, or they have
the wrong type, the GHC compiler will complain about satisfying @NumOutClocks@.
The error message on GHC 9.4 and up is:

@
    • Cannot satisfy: clash-prelude-[...]:Clash.Clocks.Internal.NumOutClocks
                        (clash-prelude-[...]:Clash.Clocks.Internal.ClocksSyncClocksInst
                           ([...])
                           DomInput) <= 7
    • In the expression: clockWizard clkIn rstIn
@

On older GHC versions, the error message is:

@
    • Couldn't match type ‘clash-prelude-[...]:Clash.Clocks.Internal.NumOutClocks
                             (clash-prelude-[...]:Clash.Clocks.Internal.ClocksSyncClocksInst
                                ([...])
                                DomInput)
                           <=? 7’
                     with ‘'True’
        arising from a use of ‘clockWizard’
    • In the expression: clockWizard clkIn rstIn
@

The above error message is also emitted when trying to instantiate more than 18
output clocks, as it will fail to find an instance. As the wizard supports no
more than 7 clocks, trying to instantiate between 8 and 18 output clocks will
also cause a type checking error. On GHC 9.4 and up, the error for attempting to
instantiate 8 clocks is:

@
    • Cannot satisfy: 8 <= 7
    • In the expression: clockWizard clkIn rstIn
@

On older GHC versions, the error message is less clear:

@
    • Couldn't match type ‘'False’ with ‘'True’
        arising from a use of ‘clockWizard’
    • In the expression: clockWizard clkIn rstIn
@
-}

{- $tcl
When generating HDL, these functions will emit a Tcl script for Vivado that
instantiates the needed IP core for the function. This Tcl script adheres to the
Clash\<-\>Tcl API. The Tcl Connector bundled in @clash-lib:Clash.DataFiles@ will
automatically process these scripts and build your design in Vivado. See
@clash-lib:Clash.DataFiles@ for more information.
-}

{- $unsafe
These functions are provided for the cases where the [regular
functions](#g:regular) cannot provide the desired behavior, like when
implementing certain advanced reset managers. These functions directly expose
the /asynchronous/ @locked@ output of the clock generator, which will assert
when the output clocks are stable. @locked@ is usually connected to reset
circuitry to keep the circuit in reset while the clock is still stabilizing.

The output of the function for /n/ output clocks is an /n+1/-tuple with /n/
clock outputs and a @locked@ signal. The compiler needs to be able to fully
determine the types of the individual tuple elements from the context; the clock
generator function itself will not constrain them. If the types of the tuple
elements cannot be inferred, you can use pattern type signatures to specify the
types. Supposing the referenced domains have been created with
'Clash.Signal.createDomain', an instance with a single output clock can be
instantiated using:

@
(clk150 :: 'Clock' Dom150, locked :: t'Clash.Signal.Signal' Dom150 'Bool') = 'unsafeClockWizard' clkIn rstIn
@

An instance with two clocks can be instantiated using

@
(clk100 :: 'Clock' Dom100
  , clk150 :: 'Clock' Dom150
  , locked :: t'Clash.Signal.Signal' Dom100 'Bool') = 'unsafeClockWizard' clkIn rstIn
@

and so on up to 7 clocks, following the general pattern @('Clock' dom1, 'Clock'
dom2, ..., 'Clock' dom/n/, t'Clash.Signal.Signal' pllLock Bool)@.

Though the @locked@ output is specified as a @t'Clash.Signal.Signal' pllLock
'Bool'@, it is an asynchronous signal and will need to be synchronized before it
can be used as a (reset) signal. While in the examples above the
@locked@ output has been assigned the domain of one of the output clocks, the
domain @pllLock@ is left unrestricted. If the lock signal is to be used in
multiple domains, the @pllLock@ domain should probably be set to @domIn@ (the
domain of the input clock and reset). While in HDL
'Clash.Explicit.Signal.unsafeSynchronizer' is just a wire, in Haskell simulation
it does actually resample the signal, and by setting @pllLock@ to @domIn@, there
is no resampling of the simulated lock signal. The simulated lock signal is
simply the inverse of the reset input: @locked@ is asserted whenever the reset
input is deasserted and vice versa.
-}

{- $unsafe_example
@
'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"DomInput\", vPeriod=20000, vResetPolarity='Clash.Signal.ActiveLow'}
'Clash.Signal.createDomain' 'Clash.Signal.vXilinxSystem'{vName=\"Dom150\", vPeriod='Clash.Signal.hzToPeriod' 150e6}

topEntity
  :: 'Clock' DomInput
  -> 'Reset' DomInput
  -> t'Clash.Signal.Signal' Dom150 Int
  -> t'Clash.Signal.Signal' Dom150 Int
topEntity clkIn rstIn = 'Clash.Signal.exposeClockResetEnable' (register 0) clk rst 'Clash.Signal.enableGen'
 where
  (clk, locked) = 'unsafeClockWizard' clkIn rstIn
  rst = 'Clash.Signal.resetSynchronizer' clk ('Clash.Signal.unsafeFromActiveLow' locked)
@

'Clash.Signal.resetSynchronizer' will keep the reset asserted when @locked@ is
'False', hence the use of @'Clash.Signal.unsafeFromActiveLow' locked@.
-}

-- | Instantiate a Xilinx MMCM clock generator corresponding to the Xilinx
-- \"Clock Wizard\" with 1 single-ended reference clock input and a reset input,
-- and 1 to 7 output clocks and a @locked@ output.
--
-- This function incorporates 'Clash.Signal.resetSynchronizer's to convert the
-- @locked@ output port into proper 'Reset' signals for the output domains which
-- will keep the circuit in reset while the clock is still stabilizing.
clockWizard ::
  forall t domIn .
  ( HasAsynchronousReset domIn
  , ClocksSyncCxt t domIn
  , NumOutClocksSync t domIn <= 7
  ) =>
  -- | Free running clock (e.g. a clock pin connected to a crystal oscillator)
  Clock domIn ->
  -- | Reset for the clock generator
  Reset domIn ->
  t
clockWizard clkIn rstIn =
  clocksResetSynchronizer (unsafeClockWizard clkIn rstIn) clkIn

-- | Instantiate a Xilinx MMCM clock generator corresponding to the Xilinx
-- \"Clock Wizard\" with 1 single-ended reference clock input and a reset input,
-- and 1 to 7 output clocks and a @locked@ output.
--
-- __NB__: Because the clock generator reacts asynchronously to the incoming
-- reset input, the signal __must__ be glitch-free.
unsafeClockWizard ::
  forall t domIn .
  ( KnownDomain domIn
  , Clocks t
  , ClocksCxt t
  , NumOutClocks t <= 7
  ) =>
  -- | Free running clock (e.g. a clock pin connected to a crystal oscillator)
  Clock domIn ->
  -- | Reset for the clock generator
  Reset domIn ->
  t
unsafeClockWizard = clocks
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unsafeClockWizard #-}
{-# ANN unsafeClockWizard hasBlackBox #-}

-- | Instantiate a Xilinx MMCM clock generator corresponding to the Xilinx
-- \"Clock Wizard\" with 1 differential reference clock input and a reset input,
-- and 1 to 7 output clocks and a @locked@ output.
--
-- This function incorporates 'Clash.Signal.resetSynchronizer's to convert the
-- @locked@ output port into proper 'Reset' signals for the output domains which
-- will keep the circuit in reset while the clock is still stabilizing.
--
-- To create a differential clock in a test bench, you can use
-- 'Clash.Explicit.Testbench.clockToDiffClock'.
clockWizardDifferential ::
  forall t domIn .
  ( HasAsynchronousReset domIn
  , ClocksSyncCxt t domIn
  , NumOutClocksSync t domIn <= 7
  ) =>
  -- | Free running clock (e.g. a clock pin pair connected to a crystal
  -- oscillator)
  DiffClock domIn ->
  -- | Reset for the clock generator
  Reset domIn ->
  t
clockWizardDifferential clkIn@(DiffClock clkInP _) rstIn =
  clocksResetSynchronizer (unsafeClockWizardDifferential clkIn rstIn) clkInP

-- | Instantiate a Xilinx MMCM clock generator corresponding to the Xilinx
-- \"Clock Wizard\" with 1 differential reference clock input and a reset input,
-- and 1 to 7 output clocks and a @locked@ output.
--
-- __NB__: Because the clock generator reacts asynchronously to the incoming
-- reset input, the signal __must__ be glitch-free.
--
-- To create a differential clock in a test bench, you can use
-- 'Clash.Explicit.Testbench.clockToDiffClock'.
unsafeClockWizardDifferential ::
  forall t domIn .
  ( KnownDomain domIn
  , Clocks t
  , ClocksCxt t
  , NumOutClocks t <= 7
  ) =>
  -- | Free running clock (e.g. a clock pin pair connected to a crystal
  -- oscillator)
  DiffClock domIn ->
  -- | Reset for the clock generator
  Reset domIn ->
  t
unsafeClockWizardDifferential (DiffClock clk _) = clocks clk
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unsafeClockWizardDifferential #-}
{-# ANN unsafeClockWizardDifferential hasBlackBox #-}
