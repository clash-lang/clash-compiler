{-|
Copyright  :  (C) 2017-2018, Google Inc
                  2019     , Myrtle Software
                  2022-2023, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

PLL and other clock-related components for Intel (Altera) FPGAs

A PLL generates a stable clock signal for your circuit at a configurable
frequency.

If you haven't determined the frequency you want the circuit to run at, the
predefined 100 MHz domain t'Clash.Signal.System' can be a good starting point.
The datasheet for your FPGA specifies lower and upper limits, but the true
maximum frequency is determined by your circuit. The 'altpllSync' and
'alteraPllSync' components below show an example for when the oscillator
connected to the FPGA runs at 50 MHz. If the oscillator runs at 100 MHz, change
@DomInput@ to:

@
'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"DomInput\", vPeriod=10000, vResetPolarity=ActiveLow}
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

-- | A clock source that corresponds to the Intel/Quartus \"ALTPLL\" component
-- (Arria GX, Arria II, Stratix IV, Stratix III, Stratix II, Stratix,
--  Cyclone 10 LP, Cyclone IV, Cyclone III, Cyclone II, Cyclone)
-- with settings to provide a stable 'Clock' from a single free-running input
--
-- This function will instantiate an \"ALTPLL\" component with 1 reference clock
-- input and a reset input, and 1 output clock and a @locked@ output. A Qsys
-- file will be generated that instructs Quartus to instantiate the correct
-- component.
--
-- This function incorporates 'Clash.Signal.resetSynchronizer' to convert the
-- @locked@ output port into a proper 'Reset' signal for the circuit which will
-- keep the circuit in reset while the clock is still stabilizing.
--
-- The PLL will react asynchronously to the incoming reset input. When the reset
-- input is asserted, the PLL's @locked@ output will deassert, in turn causing
-- the @reset@ output of @altpllSync@ to assert.
--
-- You can use 'setName' to give the IP instance a specific name, which can be
-- useful if you need to refer to the instance in Synopsys Design Constraints
-- files.
--
-- If you need access to the @locked@ output to build a more advanced reset
-- manager, you should use @unsafeAltpll@.
--
-- See also the [ALTPLL (Phase-Locked Loop) IP Core User Guide](https://www.intel.com/content/dam/www/programmable/us/en/pdfs/literature/ug/ug_altpll.pdf)
--
-- === Example
--
-- ==== Using a PLL
--
-- When the oscillator connected to the FPGA runs at 50 MHz and the external
-- reset signal is /active low/, this will generate a 100 MHz clock for the
-- t'Clash.Signal.System' domain:
--
-- @
-- 'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"DomInput\", vPeriod=20000, vResetPolarity=ActiveLow}
--
-- topEntity
--   :: 'Clock' DomInput
--   -> 'Reset' DomInput
--   -> [...]
-- topEntity clkInp rstInp = [...]
--  where
--   (clk, rst) = altpllSync \@System clkInp rstInp
-- @
--
-- Your circuit will have signals of type @'Signal' t'Clash.Signal.System'@ and
-- all the clocks and resets of your components will be the @clk@ and @rst@
-- signals generated here (modulo local resets, which will be based on @rst@ or
-- never asserted at all if the component doesn't need a reset).
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
-- (clk100, rst100) = altpllSync \@Dom100MHz clk50 rst50
-- @
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
-- This function is deprecated because the /PLL lock/ output is an asynchronous
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
  -- | Reset for the PLL
  Reset domIn ->
  -- | (Stable PLL clock, PLL lock)
  (Clock domOut, Signal domOut Bool)
altpll _ = setName @name unsafeAltpll
{-# INLINE altpll #-}
{-# DEPRECATED altpll "This function is unsafe. Please see documentation of the function for alternatives." #-}

-- | A clock source that corresponds to the Intel/Quartus \"ALTPLL\" component
-- (Arria GX, Arria II, Stratix IV, Stratix III, Stratix II, Stratix,
--  Cyclone 10 LP, Cyclone IV, Cyclone III, Cyclone II, Cyclone)
-- with settings to provide a stable 'Clock' from a single free-running input
--
-- This function is provided for the cases where 'altpllSync' cannot provide the
-- desired behavior, like when implementing certain advanced reset managers.
--
-- This function will instantiate an \"ALTPLL\" component with 1 reference clock
-- input and a reset input, and 1 output clock and a @locked@ output. A Qsys
-- file will be generated that instructs Quartus to instantiate the correct
-- component.
--
-- The PLL will react asynchronously to the incoming reset input. When the reset
-- input is asserted, the /PLL lock/ output will deassert.
--
-- The /PLL lock/ output is asserted when the clock is stable, and is usually
-- connected to reset circuitry to keep the circuit in reset while the clock is
-- still stabilizing.
--
-- Though the /PLL lock/ output is specified as a @'Signal' pllLock 'Bool'@, it
-- is an asynchronous signal and will need to be synchronized before it can be
-- used as a (reset) signal. The domain @pllLock@ is left completely free. If
-- the lock signal is to be used in multiple domains, the @pllLock@ domain
-- should probably be set to @domIn@. While in HDL
-- 'Clash.Explicit.Signal.unsafeSynchronizer' is just a wire, in Haskell
-- simulation it does actually resample the signal, and by setting @pllLock@ to
-- @domIn@, there is no resampling of the simulated lock signal. The simulated
-- lock signal is simply the inverse of the reset input: /PLL lock/ is asserted
-- whenever the reset input is deasserted and vice versa.
--
-- You can use 'setName' to give the IP instance a specific name, which can be
-- useful if you need to refer to the instance in Synopsys Design Constraints
-- files.
--
-- See also the [ALTPLL (Phase-Locked Loop) IP Core User Guide](https://www.intel.com/content/dam/www/programmable/us/en/pdfs/literature/ug/ug_altpll.pdf)
--
-- __NB__: Because the PLL reacts asynchronously to the incoming reset input,
-- the signal __must__ be glitch-free
--
-- === Example
--
-- ==== Using a PLL
--
-- When the oscillator connected to the FPGA runs at 50 MHz and the external
-- reset signal is /active low/, this will generate a 100 MHz clock for the
-- t'Clash.Signal.System' domain:
--
-- @
-- 'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"DomInput\", vPeriod=20000, vResetPolarity=ActiveLow}
--
-- topEntity
--   :: 'Clock' DomInput
--   -> 'Reset' DomInput
--   -> [...]
-- topEntity clkInp rstInp = [...]
--  where
--   (clk, pllStable) = unsafeAltpll \@System clkInp rstInp
--   rst = 'Clash.Signal.resetSynchronizer' clk ('Clash.Signal.unsafeFromActiveLow' pllStable)
-- @
--
-- 'Clash.Signal.resetSynchronizer' will keep the reset asserted when
-- @pllStable@ is 'False', hence the use of
-- @'Clash.Signal.unsafeFromActiveLow' pllStable@. Your circuit will have
-- signals of type @'Signal' t'Clash.Signal.System'@ and all the clocks and
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
-- (clk100, pllStable) = unsafeAltpll \@Dom100MHz clk50 rst50
-- @
unsafeAltpll ::
  forall t domIn .
  ( KnownDomain domIn
  , Clocks t
  , ClocksCxt t
  , NumOutClocks t <= 5
  ) =>
  -- | Free running clock (e.g. a clock pin connected to a crystal oscillator)
  Clock domIn ->
  -- | Reset for the PLL
  Reset domIn ->
  t
unsafeAltpll = clocks
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unsafeAltpll #-}
{-# ANN unsafeAltpll hasBlackBox #-}

-- | A clock source that corresponds to the Intel/Quartus \"Altera PLL\"
-- component (Arria V, Stratix V, Cyclone V) with settings to provide a stable
-- 'Clock' from a single free-running input
--
-- This function will instantiate an \"Altera PLL\" component with 1 reference
-- clock input and a reset input, and 1-16 output clocks and a @locked@ output.
-- A Qsys file will be generated that instructs Quartus to instantiate the
-- correct component.
--
-- This function incorporates 'Clash.Signal.resetSynchronizer' to convert the
-- @locked@ output port into a proper 'Reset' signal for the circuit which will
-- keep the circuit in reset while the clock is still stabilizing.
--
-- The PLL will react asynchronously to the incoming reset input. When the reset
-- input is asserted, the PLL's @locked@ output will deassert, in turn causing
-- all @reset@ outputs of @alteraPllSync@ to assert.
--
-- You can use 'setName' to give the IP instance a specific name, which can be
-- useful if you need to refer to the instance in Synopsys Design Constraints
-- files.
--
-- If you need access to the @locked@ output to build a more advanced reset
-- manager, you should use @unsafeAltpll@.
--
-- See also the [Altera Phase-Locked Loop (Altera PLL) IP Core User Guide](https://www.intel.com/content/dam/www/programmable/us/en/pdfs/literature/ug/altera_pll.pdf)
--
-- === Specifying outputs
--
-- The number of output clocks depends on this function's inferred result type.
-- The code will only type-check if the result type is a /2*n/-tuple, where each
-- two tuple elements are a 'Clock' and a 'Reset' respectively. So the general
-- shape is @('Clock' dom1, 'Reset' dom1, 'Clock' dom2, 'Reset' dom2, ...,
-- 'Clock' dom/n/, 'Reset' dom/n/)@.
--
-- An instance with a single output clock can be instantiated using:
--
-- @
-- 'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"Dom100MHz\", vPeriod=10000}
--
-- (clk100 :: 'Clock' Dom100MHz, rst100 :: 'Reset' Dom100MHz) =
--   'alteraPllSync' clk50 rst50
-- @
--
-- An instance with two clocks can be instantiated using
--
-- @
-- 'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"Dom100MHz\", vPeriod=10000}
-- 'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"Dom150MHz\", vPeriod='Clash.Signal.hzToPeriod' 150e6}
--
-- ( clk100 :: 'Clock' Dom100MHz
--  , rst100 :: 'Reset' Dom100MHz
--  , clk150 :: 'Clock' Dom150MHz
--  , rst150 :: 'Reset' Dom150MHz) = 'alteraPllSync' clk50 rst50
-- @
--
-- and so on up to 16 clocks.
--
-- This example shows how to use pattern type signatures to specify the types of
-- the clocks and the resets. This is not necessary if the types of the
-- tuple elements can already be inferred, for instance from top-level type
-- signatures.
--
-- === Example
--
-- When the oscillator connected to the FPGA runs at 50 MHz and the external
-- reset signal is /active low/, this will generate a 100 MHz clock for the
-- t'Clash.Signal.System' domain:
--
-- @
-- 'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"DomInput\", vPeriod=20000, vResetPolarity=ActiveLow}
--
-- topEntity
--   :: 'Clock' DomInput
--   -> 'Reset' DomInput
--   -> [...]
-- topEntity clkInp rstInp = [...]
--  where
--   (clk :: 'Clock' t'Clash.Signal.System', rst :: 'Reset' t'Clash.Signal.System') =
--     'alteraPllSync' clkInp rstInp
-- @
--
-- Your circuit will have signals of type @'Signal' t'Clash.Signal.System'@ and
-- all the clocks and resets of your components will be the @clk@ and @rst@
-- signals generated here (modulo local resets, which will be based on @rst@ or
-- never asserted at all if the component doesn't need a reset).
alteraPllSync ::
  forall t domIn .
  ( HasAsynchronousReset domIn
  , ClocksSyncCxt t domIn
  , NumOutClocksSync t domIn <= 18
  ) =>
  -- | Free running clock (e.g. a clock pin connected to a crystal oscillator)
  Clock domIn ->
  -- | Reset for the PLL
  Reset domIn ->
  t
alteraPllSync clkIn rstIn =
  clocksResetSynchronizer (unsafeAlteraPll clkIn rstIn) clkIn

-- | A clock source that corresponds to the Intel/Quartus \"Altera PLL\"
-- component (Arria V, Stratix V, Cyclone V) with settings to provide a stable
-- 'Clock' from a single free-running input
--
-- This function is deprecated because the /PLL lock/ output is an asynchronous
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
  -- | Reset for the PLL
  Reset domIn ->
  t
alteraPll _ = setName @name unsafeAlteraPll
{-# INLINE alteraPll #-}
{-# DEPRECATED alteraPll "This function is unsafe. Please see documentation of the function for alternatives." #-}

-- | A clock source that corresponds to the Intel/Quartus \"Altera PLL\"
-- component (Arria V, Stratix V, Cyclone V) with settings to provide a stable
-- 'Clock' from a single free-running input
--
-- This function is provided for the cases where 'alteraPllSync' cannot provide
-- the desired behavior, like when implementing certain advanced reset managers.
--
-- This function will instantiate an \"Altera PLL\" component with 1 reference
-- clock input and a reset input, and 1-16 output clocks and a @locked@ output.
-- A Qsys file will be generated that instructs Quartus to instantiate the
-- correct component.
--
-- The PLL will react asynchronously to the incoming reset input. When the reset
-- input is asserted, the /PLL lock/ output will deassert.
--
-- The /PLL lock/ output is asserted when the clocks are stable, and is usually
-- connected to reset circuitry to keep the circuit in reset while the clocks
-- are still stabilizing.
--
-- Though the /PLL lock/ output is specified as a @'Signal' pllLock 'Bool'@, it
-- is an asynchronous signal and will need to be synchronized before it can be
-- used as a (reset) signal. The domain @pllLock@ is left completely free. If
-- the lock signal is to be used in multiple domains, the @pllLock@ domain
-- should probably be set to @domIn@. While in HDL
-- 'Clash.Explicit.Signal.unsafeSynchronizer' is just a wire, in Haskell
-- simulation it does actually resample the signal, and by setting @pllLock@ to
-- @domIn@, there is no resampling of the simulated lock signal. The simulated
-- lock signal is simply the inverse of the reset input: /PLL lock/ is asserted
-- whenever the reset input is deasserted and vice versa.
--
-- You can use 'setName' to give the IP instance a specific name, which can be
-- useful if you need to refer to the instance in Synopsys Design Constraints
-- files.
--
-- See also the [Altera Phase-Locked Loop (Altera PLL) IP Core User Guide](https://www.intel.com/content/dam/www/programmable/us/en/pdfs/literature/ug/altera_pll.pdf)
--
-- __NB__: Because the PLL reacts asynchronously to the incoming reset input,
-- the signal __must__ be glitch-free
--
-- === Specifying outputs
--
-- The number of output clocks depends on this function's inferred result type.
-- The code will only type-check if the result type is an /n+1/-tuple, where the
-- first /n/ elements are 'Clock's and the final element is a
-- @'Signal' pllLock 'Bool'@. So the general shape is @('Clock' dom1, 'Clock'
-- dom2, ..., 'Clock' dom/n/, 'Signal' pllLock 'Bool')@. See above for
-- discussion of the @pllLock@ domain.
--
-- An instance with a single output clock can be instantiated using:
--
-- @
-- 'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"Dom100MHz\", vPeriod=10000}
--
-- (clk100 :: 'Clock' Dom100MHz, pllStable :: 'Signal' Dom100MHz 'Bool') =
--   'unsafeAlteraPll' clk50 rst50
-- @
--
-- An instance with two clocks can be instantiated using
--
-- @
-- 'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"Dom100MHz\", vPeriod=10000}
-- 'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"Dom150MHz\", vPeriod='Clash.Signal.hzToPeriod' 150e6}
--
-- ( clk100 :: 'Clock' Dom100MHz
--  , clk150 :: 'Clock' Dom150MHz
--  , pllStable :: 'Signal' Dom100MHz 'Bool') = 'unsafeAlteraPll' clk50 rst50
-- @
--
-- and so on up to 16 clocks.
--
-- This example shows how to use pattern type signatures to specify the types of
-- the clocks and the lock signal. This is not necessary if the types of the
-- tuple elements can already be inferred, for instance from top-level type
-- signatures.
--
-- === Example
--
-- When the oscillator connected to the FPGA runs at 50 MHz and the external
-- reset signal is /active low/, this will generate a 100 MHz clock for the
-- t'Clash.Signal.System' domain:
--
-- @
-- 'Clash.Signal.createDomain' 'Clash.Signal.vSystem'{vName=\"DomInput\", vPeriod=20000, vResetPolarity=ActiveLow}
--
-- topEntity
--   :: 'Clock' DomInput
--   -> 'Reset' DomInput
--   -> [...]
-- topEntity clkInp rstInp = [...]
--  where
--   (clk :: 'Clock' t'Clash.Signal.System', pllStable :: 'Signal' t'Clash.Signal.System' 'Bool') =
--     'unsafeAlteraPll' clkInp rstInp
--   rst = 'Clash.Signal.resetSynchronizer' clk ('Clash.Signal.unsafeFromActiveLow' pllStable)
-- @
--
-- 'Clash.Signal.resetSynchronizer' will keep the reset asserted when
-- @pllStable@ is 'False', hence the use of @'Clash.Signal.unsafeFromActiveLow'
-- pllStable@. Your circuit will have signals of type @'Signal'
-- t'Clash.Signal.System'@ and all the clocks and resets of your components will
-- be the @clk@ and @rst@ signals generated here (modulo local resets, which
-- will be based on @rst@ or never asserted at all if the component doesn't need
-- a reset).
unsafeAlteraPll ::
  forall t domIn .
  ( KnownDomain domIn
  , Clocks t
  , ClocksCxt t
  , NumOutClocks t <= 18
  ) =>
  -- | Free running clock (e.g. a clock pin connected to a crystal oscillator)
  Clock domIn ->
  -- | Reset for the PLL
  Reset domIn ->
  t
unsafeAlteraPll = clocks
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unsafeAlteraPll #-}
{-# ANN unsafeAlteraPll hasBlackBox #-}
