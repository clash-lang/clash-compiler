{-|
Copyright  :  (C) 2021,      QBayLogic B.V.,
                  2022,      Google Inc.,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for the [Xilinx Floating-Point LogiCORE IP v7.1](https://www.xilinx.com/support/documentation/ip_documentation/floating_point/v7_1/pg060-floating-point.pdf).

The functions in this module make it possible to use the Xilinx IP in Clash.
Compilation will instantiate the Xilinx IP. Clash will output a TCL script
named @floating_point@/.../@.tcl@ which needs to be executed in the Vivado
project to create the proper entity. Simulation in Clash produces bit-identical
results to synthesis.

Most functions allow customization of the Xilinx IP. Valid combinations will
need to be gleaned from, e.g., the Vivado wizard (IP Catalog -> Floating-point).
All IP instantiated by this module always has the following properties:

* Single precision
* Non-blocking
* No reset input
* No optional output fields, no @TLAST@, no @TUSER@
* @TVALID@ on the inputs is always asserted, @TVALID@ on the output is ignored.
Note that it would appear the Xilinx IP does not use these signals in
non-blocking mode anyway.
* 1 cycle per operation (meaning there need not be any dummy cycles between
consecutive inputs)

The latency of the IP is set through the delay argument of the 'DSignal'. Other
customization is done through the 'E.Config' argument of customizable functions.

De-asserting the 'Enable' signal of a function will stall the whole pipeline.

The Xilinx IP does not support calculations with subnormal numbers. All
subnormal numbers are rounded to zero on both input and output. Note that this
introduces a slight bias as the larger subnormal numbers are closer to the
smallest normal number, but they are rounded to zero nonetheless!

For each customizable operation, there also exists a function that uses the
defaults. These functions use the settings from 'E.defConfig' and the maximum
delay for the Xilinx IP with that configuration. That delay is also defined as a
type variable for delay annotation in circuits.
-}

module Clash.Cores.Xilinx.Floating
  ( -- * Instantiating IP
    addWith
  , add
  , E.AddDefDelay
  , subWith
  , sub
  , E.SubDefDelay
  , mulWith
  , mul
  , E.MulDefDelay
  , divWith
  , div
  , E.DivDefDelay
  , fromU32With
  , fromU32
  , E.FromU32DefDelay
  , E.Ordering(..)
  , E.toMaybeOrdering
  , compare
  , compareWith
  , E.CompareDefDelay
  , fromS32With
  , fromS32
  , E.FromS32DefDelay
    -- * Customizing IP
  , E.Config(..)
  , E.defConfig
  , E.ArchOpt(..)
  , E.DspUsage(..)
  , E.BMemUsage(..)
  ) where

import Clash.Prelude hiding (Ordering(..), add, sub, mul, div, compare)

import GHC.Stack (HasCallStack, withFrozenCallStack)

import qualified Clash.Cores.Xilinx.Floating.Explicit as E

-- | Customizable floating point addition.
addWith
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
addWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.addWith cfg
{-# INLINE addWith #-}

-- | Floating point addition with default settings.
add
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + E.AddDefDelay) Float
add = withFrozenCallStack $ hideEnable $ hideClock E.add
{-# INLINE add #-}

-- | Customizable floating point subtraction.
subWith
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
subWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.subWith cfg
{-# INLINE subWith #-}

-- | Floating point subtraction with default settings.
sub
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + E.SubDefDelay) Float
sub = withFrozenCallStack $ hideEnable $ hideClock E.sub
{-# INLINE sub #-}

-- | Customizable floating point multiplication.
mulWith
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
mulWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.mulWith cfg
{-# INLINE mulWith #-}

-- | Floating point multiplication with default settings.
mul
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + E.MulDefDelay) Float
mul = withFrozenCallStack $ hideEnable $ hideClock E.mul
{-# INLINE mul #-}

-- | Customizable floating point division.
divWith
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
divWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.divWith cfg
{-# INLINE divWith #-}

-- | Floating point division with default settings.
div
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + E.DivDefDelay) Float
div = withFrozenCallStack $ hideEnable $ hideClock E.div
{-# INLINE div #-}

-- | Customizable floating point comparison
--
-- Produces 'NaN' if any of the inputs is NaN. Otherwise, it behaves like
-- Haskell's 'P.compare'.
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
compareWith
  :: forall d dom n
   . ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) E.Ordering
compareWith = E.compareWith hasClock hasEnable
{-# INLINE compareWith #-}

-- | Floating point comparison, with default delay
--
-- Produces 'NaN' if any of the inputs is NaN. Otherwise, it behaves like
-- Haskell's 'P.compare'.
compare
  :: forall dom n
   . ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + E.CompareDefDelay) E.Ordering
compare = compareWith
{-# INLINE compare #-}

-- | Customizable conversion of @Unsigned 32@ to @Float@
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
fromU32With
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => DSignal dom n (Unsigned 32)
  -> DSignal dom (n + d) Float
fromU32With = withFrozenCallStack $ hideEnable $ hideClock E.fromU32With
{-# INLINE fromU32With #-}

-- | Conversion of @Unsigned 32@ to @Float@, with default delay
fromU32
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n (Unsigned 32)
  -> DSignal dom (n + E.FromU32DefDelay) Float
fromU32 = withFrozenCallStack $ hideEnable $ hideClock E.fromU32
{-# INLINE fromU32 #-}

-- | Customizable conversion of @Signed 32@ to @Float@
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
fromS32With
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => DSignal dom n (Signed 32)
  -> DSignal dom (n + d) Float
fromS32With = withFrozenCallStack $ hideEnable $ hideClock E.fromS32With
{-# INLINE fromS32With #-}

-- | Conversion of @Signed 32@ to @Float@, with default delay
fromS32
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n (Signed 32)
  -> DSignal dom (n + E.FromS32DefDelay) Float
fromS32 = withFrozenCallStack $ hideEnable $ hideClock E.fromS32
{-# INLINE fromS32 #-}
