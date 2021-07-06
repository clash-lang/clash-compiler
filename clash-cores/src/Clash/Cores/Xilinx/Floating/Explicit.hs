{-|
Copyright  :  (C) 2021,      QBayLogic B.V.,
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
customization is done through the 'Config' argument of customizable functions.

De-asserting the 'Enable' signal of a function will stall the whole pipeline.

The Xilinx IP does not support calculations with subnormal numbers. All
subnormal numbers are rounded to zero on both input and output. Note that this
introduces a slight bias as the larger subnormal numbers are closer to the
smallest normal number, but they are rounded to zero nonetheless!

For each customizable operation, there also exists a function that uses the
defaults. These functions use the settings from 'defConfig' and the maximum
delay for the Xilinx IP with that configuration. That delay is also defined as a
type variable for delay annotation in circuits.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Cores.Xilinx.Floating.Explicit
  ( -- * Instantiating IP
    addWith
  , add
  , AddDefDelay
  , subWith
  , sub
  , SubDefDelay
  , mulWith
  , mul
  , MulDefDelay
  , divWith
  , div
  , DivDefDelay
    -- * Customizing IP
  , Config(..)
  , defConfig
  , ArchOpt(..)
  , DspUsage(..)
  , BMemUsage(..)
    -- * Additional functions
  , xilinxNaN
  ) where

import Clash.Explicit.Prelude hiding (add, sub, mul, div)

import GHC.Stack (HasCallStack, withFrozenCallStack)

import Clash.Cores.Xilinx.Floating.Annotations
import Clash.Cores.Xilinx.Floating.BlackBoxes
import Clash.Cores.Xilinx.Floating.Internal

-- | Customizable floating point addition.
addWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Config
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
addWith !_ clk en (conditionFloatF -> x) (conditionFloatF -> y) =
  delayI und en clk . conditionFloatF $ x + y
 where
  und = withFrozenCallStack $ deepErrorX "Initial values of add undefined"
{-# NOINLINE addWith #-}
{-# ANN addWith (vhdlBinaryPrim 'addWith 'addTclTF "add") #-}
{-# ANN addWith (veriBinaryPrim 'addWith 'addTclTF "add") #-}

-- | Floating point addition with default settings.
add
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + AddDefDelay) Float
add = withFrozenCallStack $ addWith defConfig
{-# INLINE add #-}

-- | The default delay for floating point addition with default customization.
type AddDefDelay = 11

-- | Customizable floating point subtraction.
subWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Config
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
subWith !_ clk en (conditionFloatF -> x) (conditionFloatF -> y) =
  delayI und en clk . conditionFloatF $ x - y
 where
  und = withFrozenCallStack $ deepErrorX "Initial values of sub undefined"
{-# NOINLINE subWith #-}
{-# ANN subWith (vhdlBinaryPrim 'subWith 'subTclTF "sub") #-}
{-# ANN subWith (veriBinaryPrim 'subWith 'subTclTF "sub") #-}

-- | Floating point subtraction with default settings.
sub
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + SubDefDelay) Float
sub = withFrozenCallStack $ subWith defConfig
{-# INLINE sub #-}

-- | The default delay for floating point subtraction with default
-- customization.
type SubDefDelay = 11

-- | Customizable floating point multiplication.
mulWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Config
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
mulWith !_ clk en (conditionFloatF -> x) (conditionFloatF -> y) =
  delayI und en clk . conditionFloatF $ x * y
 where
  und = withFrozenCallStack $ deepErrorX "Initial values of mul undefined"
{-# NOINLINE mulWith #-}
{-# ANN mulWith (vhdlBinaryPrim 'mulWith 'mulTclTF "mul") #-}
{-# ANN mulWith (veriBinaryPrim 'mulWith 'mulTclTF "mul") #-}

-- | Floating point multiplication with default settings.
mul
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + MulDefDelay) Float
mul = withFrozenCallStack $ mulWith defConfig
{-# INLINE mul #-}

-- | The default delay for floating point multiplication with default
-- customization.
type MulDefDelay = 8

-- | Customizable floating point division.
divWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Config
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
divWith !_ clk en (conditionFloatF -> x) (conditionFloatF -> y) =
  delayI und en clk . conditionFloatF $ x / y
 where
  und = withFrozenCallStack $ deepErrorX "Initial values of div undefined"
{-# NOINLINE divWith #-}
{-# ANN divWith (vhdlBinaryPrim 'divWith 'divTclTF "div") #-}
{-# ANN divWith (veriBinaryPrim 'divWith 'divTclTF "div") #-}

-- | Floating point division with default settings.
div
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + DivDefDelay) Float
div = withFrozenCallStack $ divWith defConfig
{-# INLINE div #-}

-- | The default delay for floating point division with default customization.
type DivDefDelay = 28

-- | Default customization options.
--
-- For those operations that support it, the default options are:
--
-- * Speed-optimized architecture ('SpeedArch')
-- * Full DSP slice usage ('FullDspUsage')
-- * Exponential operator does not use block memory ('NoBMemUsage')
defConfig :: Config
defConfig = Config
  { archOpt = SpeedArch
  , dspUsage = FullDspUsage
  , bMemUsage = NoBMemUsage
  }
