{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017     , Google Inc.
                  2019     , Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

This module defines the explicitly clocked counterparts of the functions
defined in "Clash.Prelude".
-}

{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE Unsafe #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK show-extensions, not-home #-}

module Clash.Explicit.Prelude
  ( -- * Creating synchronous sequential circuits
    mealy
  , mealyB
  , moore
  , mooreB
  , registerB
    -- * Synchronizer circuits for safe clock domain crossings
  , dualFlipFlopSynchronizer
  , asyncFIFOSynchronizer
    -- * ROMs
  , asyncRom
  , asyncRomPow2
  , rom
  , romPow2
    -- ** ROMs initialized with a data file
  , asyncRomFile
  , asyncRomFilePow2
  , romFile
  , romFilePow2
    -- * RAM primitives with a combinational read port
  , asyncRam
  , asyncRamPow2
    -- * BlockRAM primitives
  , blockRam
  , blockRamPow2
  , blockRamU
  , blockRam1
  , ResetStrategy(..)
    -- ** BlockRAM primitives initialized with a data file
  , blockRamFile
  , blockRamFilePow2
  -- ** BlockRAM read/write conflict resolution
  , readNew
    -- * Utility functions
  , window
  , windowD
  , isRising
  , isFalling
  , riseEvery
  , oscillate
    -- * Testbench functions
  , assert
  , stimuliGenerator
  , outputVerifier'
    -- * Tracing
    -- ** Simple
  , traceSignal1
  , traceVecSignal1
    -- ** Tracing in a multi-clock environment
  , traceSignal
  , traceVecSignal
    -- ** VCD dump functions
  , dumpVCD
    -- * Exported modules
    -- ** Synchronous signals
  , module Clash.Explicit.Reset
  , module Clash.Explicit.Signal
  , module Clash.Explicit.Signal.Delayed
    -- ** Datatypes
    -- *** Bit vectors
  , module Clash.Sized.BitVector
  , module Clash.Prelude.BitIndex
  , module Clash.Prelude.BitReduction
    -- *** Arbitrary-width numbers
  , module Clash.Sized.Signed
  , module Clash.Sized.Unsigned
  , module Clash.Sized.Index
    -- *** Fixed point numbers
  , module Clash.Sized.Fixed
    -- *** Fixed size vectors
  , module Clash.Sized.Vector
    -- *** Perfect depth trees
  , module Clash.Sized.RTree
    -- ** Annotations
  , module Clash.Annotations.TopEntity
    -- ** Generics type-classes
  , Generic
  , Generic1
    -- ** Type-level natural numbers
  , module GHC.TypeLits
  , module GHC.TypeLits.Extra
  , module Clash.Promoted.Nat
  , module Clash.Promoted.Nat.Literals
  , module Clash.Promoted.Nat.TH
    -- ** Type-level strings
  , module Clash.Promoted.Symbol
    -- ** Template Haskell
  , Lift (..)
    -- ** Type classes
    -- *** Clash
  , module Clash.Class.AutoReg
  , module Clash.Class.BitPack
  , module Clash.Class.Exp
  , module Clash.Class.Num
  , module Clash.Class.Resize
    -- *** Other
  , module Control.Applicative
  , module Data.Bits
  , module Data.Default.Class
    -- ** Exceptions
  , module Clash.XException
    -- ** Named types
  , module Clash.NamedTypes
    -- ** Magic
  , module Clash.Magic
    -- ** Haskell Prelude
    -- $hiding
  , module Clash.HaskellPrelude
  )
where

import Control.Applicative
import Data.Bits
import Data.Default.Class
import GHC.TypeLits
import GHC.TypeLits.Extra
import Language.Haskell.TH.Syntax  (Lift(..))
import Clash.HaskellPrelude

import Clash.Annotations.TopEntity
import Clash.Class.AutoReg
import Clash.Class.BitPack hiding (GBitPack(..))
import Clash.Class.Exp
import Clash.Class.Num
import Clash.Class.Resize
import Clash.Magic
import Clash.NamedTypes
import Clash.Explicit.BlockRam
import Clash.Explicit.BlockRam.File
import Clash.Explicit.Mealy
import Clash.Explicit.Moore
import Clash.Explicit.RAM
import Clash.Explicit.ROM
import Clash.Explicit.ROM.File
import Clash.Explicit.Prelude.Safe
import Clash.Explicit.Reset
import Clash.Explicit.Signal
import Clash.Explicit.Signal.Delayed
import Clash.Explicit.Testbench
import Clash.Prelude.BitIndex
import Clash.Prelude.BitReduction
import Clash.Prelude.ROM.File       (asyncRomFile, asyncRomFilePow2)
import Clash.Promoted.Nat
import Clash.Promoted.Nat.TH
import Clash.Promoted.Nat.Literals
import Clash.Promoted.Symbol
import Clash.Signal.Trace
import Clash.Sized.BitVector
import Clash.Sized.Fixed
import Clash.Sized.Index
import Clash.Sized.RTree
import Clash.Sized.Signed
import Clash.Sized.Unsigned
import Clash.Sized.Vector hiding (fromList, unsafeFromList)
import Clash.XException

{- $setup
>>> :set -XDataKinds -XTypeApplications
>>> :m -Clash.Prelude
>>> import Clash.Explicit.Prelude
>>> let window4 = window @3
>>> let windowD3 = windowD @2
-}

{- $hiding
"Clash.Explicit.Prelude" re-exports most of the Haskell "Prelude" with the
exception of those functions that the Clash API defines to work on 'Vec' from
"Clash.Sized.Vector" instead of on lists as the Haskell Prelude does. In
addition, for the 'Clash.Class.Parity.odd' and 'Clash.Class.Parity.even'
functions a type class called 'Clash.Class.Parity.Parity' is available at
"Clash.Class.Parity".
-}

-- | Give a window over a 'Signal'
--
-- @
-- window4
---  :: Clock dom
--   -> Reset dom
--   -> Enable dom
--   -> 'Signal' dom Int
--   -> 'Vec' 4 ('Signal' dom Int)
-- window4 = 'window'
-- @
--
-- >>> simulateB (window4 systemClockGen systemResetGen enableGen) [1::Int,2,3,4,5] :: [Vec 4 Int]
-- [<1,0,0,0>,<2,1,0,0>,<3,2,1,0>,<4,3,2,1>,<5,4,3,2>...
-- ...
window
  :: ( KnownNat n
     , KnownDomain dom
     , NFDataX a
     , Default a
     )
  => Clock dom
  -- ^ Clock to the incoming signal is synchronized
  -> Reset dom
  -> Enable dom
  -> Signal dom a
  -- ^ Signal to create a window over
  -> Vec (n + 1) (Signal dom a)
  -- ^ Window of at least size 1
window clk rst en x = res
  where
    res  = x :> prev
    prev = case natVal (asNatProxy prev) of
             0 -> repeat def
             _ -> let next = x +>> prev
                  in  registerB clk rst en (repeat def) next
{-# INLINABLE window #-}

-- | Give a delayed window over a 'Signal'
--
-- @
-- windowD3
--   :: KnownDomain dom
--   -> Clock dom
--   -> Enable dom
--   -> Reset dom
--   -> 'Signal' dom Int
--   -> 'Vec' 3 ('Signal' dom Int)
-- windowD3 = 'windowD'
-- @
--
-- >>> simulateB (windowD3 systemClockGen resetGen enableGen) [1::Int,1,2,3,4] :: [Vec 3 Int]
-- [<0,0,0>,<0,0,0>,<1,0,0>,<2,1,0>,<3,2,1>,<4,3,2>...
-- ...
windowD
  :: ( KnownNat n
     , NFDataX a
     , Default a
     , KnownDomain dom )
  => Clock dom
  -- ^ Clock to which the incoming signal is synchronized
  -> Reset dom
  -> Enable dom
  -> Signal dom a
  -- ^ Signal to create a window over
  -> Vec (n + 1) (Signal dom a)
  -- ^ Window of at least size 1
windowD clk rst en x =
  let prev = registerB clk rst en (repeat def) next
      next = x +>> prev
  in  prev
{-# INLINABLE windowD #-}
