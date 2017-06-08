{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017     , Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

This module defines the explicitly clocked counterparts of the functions
defined in "CLaSH.Prelude". Take a look at "CLaSH.Signal.Explicit" to see how
you can make multi-clock designs.
-}

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE Unsafe #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module CLaSH.Explicit.Prelude
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
    -- ** ROMs initialised with a data file
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
    -- ** BlockRAM primitives initialised with a data file
  , blockRamFile
  , blockRamFilePow2
  -- ** BlockRAM read/write conflict resolution
  , readNew
    -- * Utility functions
  , window
  , windowD
  , isRising
  , isFalling
    -- * Testbench functions
  , assert
  , stimuliGenerator
  , outputVerifier
    -- * Exported modules
    -- ** Synchronous signals
  , module CLaSH.Explicit.Signal
  , module CLaSH.Explicit.Signal.Delayed
    -- ** DataFlow interface
  , module CLaSH.Prelude.DataFlow
    -- ** Datatypes
    -- *** Bit vectors
  , module CLaSH.Sized.BitVector
  , module CLaSH.Prelude.BitIndex
  , module CLaSH.Prelude.BitReduction
    -- *** Arbitrary-width numbers
  , module CLaSH.Sized.Signed
  , module CLaSH.Sized.Unsigned
  , module CLaSH.Sized.Index
    -- *** Fixed point numbers
  , module CLaSH.Sized.Fixed
    -- *** Fixed size vectors
  , module CLaSH.Sized.Vector
    -- *** Perfect depth trees
  , module CLaSH.Sized.RTree
    -- ** Annotations
  , module CLaSH.Annotations.TopEntity
    -- ** Type-level natural numbers
  , module GHC.TypeLits
  , module GHC.TypeLits.Extra
  , module CLaSH.Promoted.Nat
  , module CLaSH.Promoted.Nat.Literals
  , module CLaSH.Promoted.Nat.TH
  , module CLaSH.Promoted.Symbol
    -- ** Template Haskell
  , Lift (..)
    -- ** Type classes
    -- *** CLaSH
  , module CLaSH.Class.BitPack
  , module CLaSH.Class.Num
  , module CLaSH.Class.Resize
    -- *** Other
  , module Control.Applicative
  , module Data.Bits
  , module Data.Default
    -- ** Exceptions
  , module CLaSH.XException
  , undefined
    -- ** Named types
  , module CLaSH.NamedTypes
    -- ** Haskell Prelude
    -- $hiding
  , module Prelude
  )
where

import Control.Applicative
import Data.Bits
import Data.Default
import GHC.TypeLits
import GHC.TypeLits.Extra
import Language.Haskell.TH.Syntax  (Lift(..))
import Prelude hiding
  ((++), (!!), concat, drop, foldl, foldl1, foldr, foldr1, head, init, iterate,
   last, length, map, repeat, replicate, reverse, scanl, scanr, splitAt, tail,
   take, unzip, unzip3, zip, zip3, zipWith, zipWith3, undefined)

import CLaSH.Annotations.TopEntity
import CLaSH.Class.BitPack
import CLaSH.Class.Num
import CLaSH.Class.Resize
import CLaSH.NamedTypes
import CLaSH.Explicit.BlockRam
import CLaSH.Explicit.BlockRam.File
import CLaSH.Explicit.Mealy
import CLaSH.Explicit.Moore
import CLaSH.Explicit.RAM
import CLaSH.Explicit.ROM
import CLaSH.Explicit.ROM.File
import CLaSH.Explicit.Prelude.Safe
import CLaSH.Explicit.Signal
import CLaSH.Explicit.Signal.Delayed
import CLaSH.Explicit.Synchronizer
  (dualFlipFlopSynchronizer, asyncFIFOSynchronizer)
import CLaSH.Explicit.Testbench
import CLaSH.Prelude.BitIndex
import CLaSH.Prelude.BitReduction
import CLaSH.Prelude.DataFlow
import CLaSH.Prelude.ROM            (asyncRom, asyncRomPow2)
import CLaSH.Prelude.ROM.File       (asyncRomFile, asyncRomFilePow2)
import CLaSH.Promoted.Nat
import CLaSH.Promoted.Nat.TH
import CLaSH.Promoted.Nat.Literals
import CLaSH.Promoted.Symbol
import CLaSH.Sized.BitVector
import CLaSH.Sized.Fixed
import CLaSH.Sized.Index
import CLaSH.Sized.RTree
import CLaSH.Sized.Signed
import CLaSH.Sized.Unsigned
import CLaSH.Sized.Vector
import CLaSH.XException

{- $setup
>>> :set -XDataKinds -XTypeApplications
>>> import CLaSH.Explicit.Prelude
>>> let window4 = window @3
>>> let windowD3 = windowD @2
-}

-- | Give a window over a 'Signal'
--
-- @
-- window4
---  :: Clock domain gated -> Reset domain synchronous
--   -> 'Signal' domain Int -> 'Vec' 4 ('Signal' domain Int)
-- window4 = 'window'
-- @
--
-- >>> simulateB (window4 systemClock systemReset) [1::Int,2,3,4,5] :: [Vec 4 Int]
-- [<1,0,0,0>,<2,1,0,0>,<3,2,1,0>,<4,3,2,1>,<5,4,3,2>...
-- ...
window
  :: (KnownNat n, Default a)
  => Clock domain gated
  -- ^ Clock to which the incoming signal is synchronized
  -> Reset domain synchronous
  -> Signal domain a               -- ^ Signal to create a window over
  -> Vec (n + 1) (Signal domain a) -- ^ Window of at least size 1
window clk rst x = res
  where
    res  = x :> prev
    prev = case natVal (asNatProxy prev) of
             0 -> repeat def
             _ -> let next = x +>> prev
                  in  registerB clk rst (repeat def) next
{-# INLINABLE window #-}

-- | Give a delayed window over a 'Signal'
--
-- @
-- windowD3 :: Clock domain gated -> Reset domain synchronous
--          -> 'Signal' domain Int -> 'Vec' 3 ('Signal' domain Int)
-- windowD3 = 'windowD'
-- @
--
-- >>> simulateB (windowD3 systemClock systemReset) [1::Int,2,3,4] :: [Vec 3 Int]
-- [<0,0,0>,<1,0,0>,<2,1,0>,<3,2,1>,<4,3,2>...
-- ...
windowD
  :: (KnownNat n, Default a)
  => Clock domain gated
  -- ^ Clock to which the incoming signal is synchronized
  -> Reset domain synchronous
  -> Signal domain a                -- ^ Signal to create a window over
  -> Vec (n + 1) (Signal domain a)  -- ^ Window of at least size 1
windowD clk rst x =
  let prev = registerB clk rst (repeat def) next
      next = x +>> prev
  in  prev
{-# INLINABLE windowD #-}
