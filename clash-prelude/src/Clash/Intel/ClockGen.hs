{-|
Copyright  :  (C) 2017-2018, Google Inc
                  2019     , Myrtle Software
                  2022-2023, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

This module contains functions for instantiating clock generators on Intel
FPGA's.

We suggest you use a clock generator even if your oscillator runs at the
frequency you want to run your circuit at.

A clock generator generates a stable clock signal for your design at a
configurable frequency. A clock generator in an FPGA is frequently referred to
as a PLL (Phase-Locked Loop). Intel also refers to them as PLL's in general but
because this is not consistently the case among FPGA vendors, we choose the
more generic term /clock generator/.

For most use cases, you would create two or more synthesis domains describing
the oscillator input and the domains you wish to use in your design, and use
the [regular functions](#g:regular) below to generate the clocks and resets of
the design from the oscillator input. There are use cases not covered by this
simpler approach, and the [unsafe functions](#g:unsafe) are provided as a means
to build advanced reset managers for the output domains.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.Intel.ClockGen
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

    -- * Regular functions #regular#
    altpllSync
  , alteraPllSync
    -- * Unsafe functions #unsafe#
    -- $unsafe

    -- ** Example
    -- $unsafe_example
  , unsafeAltpll
  , unsafeAlteraPll
    -- * Deprecated
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
predefined 100 MHz domain t'Clash.Signal.System' can be a good starting point.
The datasheet for your FPGA specifies lower and upper limits, but the true
maximum frequency is determined by your design.

Supposing you need a clock running at 150 MHz for your design, the following
will instantiate a suitable domain:

@
'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"Dom150\", vPeriod='Clash.Signal.hzToPeriod' 150e6}
@

As the clock generator always reacts asynchronously to its reset input, it will
require that the @DomInput@ domain has asynchronous resets. The /unsafe/
functions below do not enforce this requirement on the domain (but they still
react asynchronously).
-}

{- $caution
The clock generator in the FPGA is limited in which clock frequencies it can
generate, especially when one clock generator has multiple outputs. The clock
generator will pick the attainable frequency closest to the requested frequency
(or possibly fail to synthesize). You can check the frequency that the IP core
chose by loading your design into the Quartus GUI. In the /Project Navigator/,
choose the /Hierarchy/ view and find your clock generator instance.
Double-click the instance to open Platform Designer and choose /Edit/
/Parameters.../. In the /Output Clocks/ page, the relevant column is /Actual/
/Settings/. If the actual value differs, copy the actual value back to the
Clash design.
-}

{- $using
The functions in this module will instantiate an Intel IP core for a clock
generator with 1 reference clock input and a reset input, and one or more output
clocks and a @locked@ output.

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
(clk150 :: 'Clock' Dom150, rst150 :: 'Reset' Dom150) = 'alteraPllSync' clkIn rstIn
@

An instance with two clocks can be instantiated using

@
( clk100 :: 'Clock' Dom100
  , rst100 :: 'Reset' Dom100
  , clk150 :: 'Clock' Dom150
  , rst150 :: 'Reset' Dom150) = 'alteraPllSync' clkIn rstIn
@

and so on up to 18 clocks, following the general pattern @('Clock' dom1, 'Reset'
dom1, 'Clock' dom2, 'Reset' dom2, ..., 'Clock' dom/n/, 'Reset' dom/n/)@.

These examples show 'alteraPllSync' but it is the same for 'altpllSync' except
that it supports up to 5 clocks.

If you need access to the @locked@ output to build a more advanced reset
manager, you should use the [unsafe functions](#g:unsafe) instead.
-}

{- $example

When the oscillator connected to the FPGA runs at 50 MHz and the external reset
signal is /active-low/, this will generate a 150 MHz clock for use by the
circuit:

@
'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"DomInput\", vPeriod=20000, vResetPolarity='Clash.Signal.ActiveLow'}
'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"Dom150\", vPeriod='Clash.Signal.hzToPeriod' 150e6}

topEntity
  :: 'Clock' DomInput
  -> 'Reset' DomInput
  -> t'Clash.Signal.Signal' Dom150 Int
  -> t'Clash.Signal.Signal' Dom150 Int
topEntity clkIn rstIn = 'Clash.Signal.exposeClockResetEnable' (register 0) clk rst 'Clash.Signal.enableGen'
 where
  (clk, rst) = 'alteraPllSync' clkIn rstIn
@
-}

{- $error
When type checking cannot infer the types of the tuple elements, or they have
the wrong type, the GHC compiler will complain about satisfying @NumOutClocks@.
The error message on GHC 9.4 and up is:

>     • Cannot satisfy: clash-prelude-[...]:Clash.Clocks.Internal.NumOutClocks
>                         (clash-prelude-[...]:Clash.Clocks.Internal.ClocksSyncClocksInst
>                            ([...])
>                            DomInput) <= 18
>     • In the expression: alteraPllSync clkIn rstIn

On older GHC versions, the error message is:

>     • Couldn't match type ‘clash-prelude-[...]:Clash.Clocks.Internal.NumOutClocks
>                              (clash-prelude-[...]:Clash.Clocks.Internal.ClocksSyncClocksInst
>                                 ([...])
>                                 DomInput)
>                            <=? 18’
>                      with ‘'True’
>         arising from a use of ‘alteraPllSync’
>     • In the expression: alteraPllSync clkIn rstIn

The above error message is also emitted when trying to instantiate more than 18
output clocks, as it will fail to find an instance. As 'altpllSync' supports no
more than 5 clocks, trying to instantiate between 6 and 18 output clocks will
also cause a type checking error. On GHC 9.4 and up, the error for attempting to
instantiate 6 clocks is:

>     • Cannot satisfy: 6 <= 5
>     • In the expression: altpllSync clkIn rstIn

On older GHC versions, the error message is less clear:

>     • Couldn't match type ‘'False’ with ‘'True’
>         arising from a use of ‘altpllSync’
>     • In the expression: altpllSync clkIn rstIn
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
(clk150 :: 'Clock' Dom150, locked :: t'Clash.Signal.Signal' Dom150 'Bool') = 'unsafeAlteraPll' clkIn rstIn
@

An instance with two clocks can be instantiated using

@
(clk100 :: 'Clock' Dom100
  , clk150 :: 'Clock' Dom150
  , locked :: t'Clash.Signal.Signal' Dom100 'Bool') = 'unsafeAlteraPll' clkIn rstIn
@

and so on up to 18 clocks, following the general pattern @('Clock' dom1, 'Clock'
dom2, ..., 'Clock' dom/n/, t'Clash.Signal.Signal' pllLock Bool)@.

These examples show 'unsafeAlteraPll' but it is the same for 'unsafeAltpll'
except that it supports up to 5 clocks.

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
'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"Dom150\", vPeriod='Clash.Signal.hzToPeriod' 150e6}

topEntity
  :: 'Clock' DomInput
  -> 'Reset' DomInput
  -> t'Clash.Signal.Signal' Dom150 Int
  -> t'Clash.Signal.Signal' Dom150 Int
topEntity clkIn rstIn = 'Clash.Signal.exposeClockResetEnable' (register 0) clk rst 'Clash.Signal.enableGen'
 where
  (clk, locked) = 'unsafeAlteraPll' clkIn rstIn
  rst = 'Clash.Signal.resetSynchronizer' clk ('Clash.Signal.unsafeFromActiveLow' locked)
@

'Clash.Signal.resetSynchronizer' will keep the reset asserted when @locked@ is
'False', hence the use of @'Clash.Signal.unsafeFromActiveLow' locked@.
-}

-- | Instantiate an Intel clock generator corresponding to the Intel/Quartus
-- \"ALTPLL\" IP core (Arria GX, Arria II, Stratix IV, Stratix III, Stratix II,
-- Stratix, Cyclone 10 LP, Cyclone IV, Cyclone III, Cyclone II, Cyclone) with 1
-- reference clock input and a reset input and 1 to 5 output clocks and a
-- @locked@ output.
--
-- This function incorporates 'Clash.Signal.resetSynchronizer's to convert the
-- @locked@ output port into proper 'Reset' signals for the output domains which
-- will keep the circuit in reset while the clock is still stabilizing.
--
-- See also the [ALTPLL (Phase-Locked Loop) IP Core User Guide](https://www.intel.com/content/dam/www/programmable/us/en/pdfs/literature/ug/ug_altpll.pdf)
altpllSync ::
  forall t domIn .
  ( HasAsynchronousReset domIn
  , ClocksSyncCxt t domIn
  , NumOutClocksSync t domIn <= 5
  ) =>
  -- | Free running clock (e.g. a clock pin connected to a crystal oscillator)
  Clock domIn ->
  -- | Reset for the clock generator
  Reset domIn ->
  t
altpllSync clkIn rstIn =
  clocksResetSynchronizer (unsafeAltpll clkIn rstIn) clkIn

-- | Instantiate an Intel clock generator corresponding to the Intel/Quartus
-- \"ALTPLL\" IP core (Arria GX, Arria II, Stratix IV, Stratix III, Stratix II,
-- Stratix, Cyclone 10 LP, Cyclone IV, Cyclone III, Cyclone II, Cyclone) with 1
-- reference clock input and a reset input and 1 output clock and a @locked@
-- output.
--
-- This function is deprecated because the @locked@ output is an asynchronous
-- signal. This means the user is required to add a synchronizer and as such
-- this function is unsafe. The common use case is now covered by 'altpllSync'
-- and 'unsafeAltpll' offers the functionality of this deprecated function for
-- advanced use cases.
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
  -- | Reset for the clock generator
  Reset domIn ->
  -- | (Output clock, Clock generator locked)
  (Clock domOut, Signal domOut Bool)
altpll _ = setName @name unsafeAltpll
{-# INLINE altpll #-}
{-# DEPRECATED altpll "This function is unsafe. Please see documentation of the function for alternatives." #-}

-- | Instantiate an Intel clock generator corresponding to the Intel/Quartus
-- \"ALTPLL\" IP core (Arria GX, Arria II, Stratix IV, Stratix III, Stratix II,
-- Stratix, Cyclone 10 LP, Cyclone IV, Cyclone III, Cyclone II, Cyclone) with 1
-- reference clock input and a reset input and 1 to 5 output clocks and a
-- @locked@ output.
--
-- __NB__: Because the clock generator reacts asynchronously to the incoming
-- reset input, the signal __must__ be glitch-free.
--
-- See also the [ALTPLL (Phase-Locked Loop) IP Core User Guide](https://www.intel.com/content/dam/www/programmable/us/en/pdfs/literature/ug/ug_altpll.pdf)
unsafeAltpll ::
  forall t domIn .
  ( KnownDomain domIn
  , Clocks t
  , ClocksCxt t
  , NumOutClocks t <= 5
  ) =>
  -- | Free running clock (e.g. a clock pin connected to a crystal oscillator)
  Clock domIn ->
  -- | Reset for the clock generator
  Reset domIn ->
  t
unsafeAltpll = clocks
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unsafeAltpll #-}
{-# ANN unsafeAltpll hasBlackBox #-}

-- | Instantiate an Intel clock generator corresponding to the Intel/Quartus
-- \"Altera PLL\" IP core (Arria V, Stratix V, Cyclone V) with 1 reference clock
-- input and a reset input and 1 to 18 output clocks and a @locked@ output.
--
-- This function incorporates 'Clash.Signal.resetSynchronizer's to convert the
-- @locked@ output port into proper 'Reset' signals for the output domains which
-- will keep the circuit in reset while the clock is still stabilizing.
--
-- See also the [Altera Phase-Locked Loop (Altera PLL) IP Core User Guide](https://www.intel.com/content/dam/www/programmable/us/en/pdfs/literature/ug/altera_pll.pdf)
alteraPllSync ::
  forall t domIn .
  ( HasAsynchronousReset domIn
  , ClocksSyncCxt t domIn
  , NumOutClocksSync t domIn <= 18
  ) =>
  -- | Free running clock (e.g. a clock pin connected to a crystal oscillator)
  Clock domIn ->
  -- | Reset for the clock generator
  Reset domIn ->
  t
alteraPllSync clkIn rstIn =
  clocksResetSynchronizer (unsafeAlteraPll clkIn rstIn) clkIn

-- | Instantiate an Intel clock generator corresponding to the Intel/Quartus
-- \"Altera PLL\" IP core (Arria V, Stratix V, Cyclone V) with 1 reference clock
-- input and a reset input and 1 to 18 output clocks and a @locked@ output.
--
-- This function is deprecated because the @locked@ output is an asynchronous
-- signal. This means the user is required to add a synchronizer and as such
-- this function is unsafe. The common use case is now covered by
-- 'alteraPllSync' and 'unsafeAlteraPll' offers the functionality of this
-- deprecated function for advanced use cases.
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
  -- | Reset for the clock generator
  Reset domIn ->
  t
alteraPll _ = setName @name unsafeAlteraPll
{-# INLINE alteraPll #-}
{-# DEPRECATED alteraPll "This function is unsafe. Please see documentation of the function for alternatives." #-}

-- | Instantiate an Intel clock generator corresponding to the Intel/Quartus
-- \"Altera PLL\" IP core (Arria V, Stratix V, Cyclone V) with 1 reference clock
-- input and a reset input and 1 to 18 output clocks and a @locked@ output.
--
-- __NB__: Because the clock generator reacts asynchronously to the incoming
-- reset input, the signal __must__ be glitch-free.
--
-- See also the [Altera Phase-Locked Loop (Altera PLL) IP Core User Guide](https://www.intel.com/content/dam/www/programmable/us/en/pdfs/literature/ug/altera_pll.pdf)
unsafeAlteraPll ::
  forall t domIn .
  ( KnownDomain domIn
  , Clocks t
  , ClocksCxt t
  , NumOutClocks t <= 18
  ) =>
  -- | Free running clock (e.g. a clock pin connected to a crystal oscillator)
  Clock domIn ->
  -- | Reset for the clock generator
  Reset domIn ->
  t
unsafeAlteraPll = clocks
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unsafeAlteraPll #-}
{-# ANN unsafeAlteraPll hasBlackBox #-}
