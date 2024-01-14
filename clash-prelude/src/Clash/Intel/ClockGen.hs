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

{-# LANGUAGE FlexibleContexts #-}

module Clash.Intel.ClockGen
  ( altpll
  , alteraPll
  ) where

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Clocks           (Clocks (..))
import Clash.Promoted.Symbol  (SSymbol)
import Clash.Signal.Internal
  (Signal, Clock, Reset, KnownDomain (..))


<<<<<<< HEAD
-- | A clock source that corresponds to the Intel/Quartus \"ALTPLL\" component
-- (Arria GX, Arria II, Stratix IV, Stratix III, Stratix II, Stratix,
--  Cyclone 10 LP, Cyclone IV, Cyclone III, Cyclone II, Cyclone)
-- with settings to provide a stable 'Clock' from a single free-running input
=======
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
>>>>>>> cb401b8c5 (Haddock: Fix very confusing formatting errors (#2622))
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
--            ('Clash.Signal.unsafeFromLowPolarity' rstInp)
--   rst = 'Clash.Signal.resetSynchronizer' clk ('Clash.Signal.unsafeFromLowPolarity' pllStable)
-- @
--
-- 'Clash.Signal.resetSynchronizer' will keep the reset asserted when
-- @pllStable@ is 'False', hence the use of
-- @'Clash.Signal.unsafeFromLowPolarity' pllStable@. Your circuit will have
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
altpll
  :: forall domOut domIn name
   . (KnownDomain domIn, KnownDomain domOut)
  => SSymbol name
  -- ^ Name of the component instance
  --
  -- Instantiate as follows: @(SSymbol \@\"altpll50\")@
  -> Clock domIn
  -- ^ Free running clock (e.g. a clock pin connected to a crystal oscillator)
  -> Reset domIn
  -- ^ Reset for the PLL
  -> (Clock domOut, Signal domOut Bool)
  -- ^ (Stable PLL clock, PLL lock)
altpll !_ = knownDomain @domIn `seq` knownDomain @domOut `seq` clocks
{-# NOINLINE altpll #-}
{-# ANN altpll hasBlackBox #-}

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
--               ('Clash.Signal.unsafeFromLowPolarity' rstInp)
--   rst = 'Clash.Signal.resetSynchronizer' clk ('Clash.Signal.unsafeFromLowPolarity' pllStable)
-- @
--
-- 'Clash.Signal.resetSynchronizer' will keep the reset asserted when
-- @pllStable@ is 'False', hence the use of
-- @'Clash.Signal.unsafeFromLowPolarity' pllStable@. Your circuit will have
-- signals of type @'Signal' 'Clash.Signal.System'@ and all the clocks and
-- resets of your components will be the @clk@ and @rst@ signals generated here
-- (modulo local resets, which will be based on @rst@ or never asserted at all
-- if the component doesn't need a reset).
alteraPll
  :: (Clocks t, KnownDomain domIn, ClocksCxt t)
  => SSymbol name
  -- ^ Name of the component instance
  --
  -- Instantiate as follows: @(SSymbol \@\"alterapll50\")@
  -> Clock domIn
  -- ^ Free running clock (e.g. a clock pin connected to a crystal oscillator)
  -> Reset domIn
  -- ^ Reset for the PLL
  -> t
alteraPll !_ = clocks
{-# NOINLINE alteraPll #-}
{-# ANN alteraPll hasBlackBox #-}
