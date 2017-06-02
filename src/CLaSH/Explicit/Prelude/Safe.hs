{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017     , Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

__This is the <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/safe-haskell.html Safe> API only of "CLaSH.Prelude.Explicit"__

This module defines the explicitly clocked counterparts of the functions
defined in "CLaSH.Prelude".

This module uses the explicitly clocked 'Signal'' synchronous signals, as
opposed to the implicitly clocked 'Signal' used in "CLaSH.Prelude". Take a
look at "CLaSH.Signal.Explicit" to see how you can make multi-clock designs
using explicitly clocked signals.
-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module CLaSH.Explicit.Prelude.Safe
  ( -- * Creating synchronous sequential circuits
    mealy
  , mealyB
  , moore
  , mooreB
  , registerB
    -- * Synchronizer circuits for safe clock domain crossing
  , dualFlipFlopSynchronizer
  , asyncFIFOSynchronizer
    -- * ROMs
  , asyncRom
  , asyncRomPow2
  , rom
  , romPow2
    -- * RAM primitives with a combinational read port
  , asyncRam
  , asyncRamPow2
    -- * BlockRAM primitives
  , blockRam
  , blockRamPow2
    -- ** BlockRAM read/write conflict resolution
  , readNew
    -- * Utility functions
  , isRising
  , isFalling
  , riseEvery
  , oscillate
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
    -- ** Type classes
    -- *** CLaSH
  , module CLaSH.Class.BitPack
  , module CLaSH.Class.Num
  , module CLaSH.Class.Resize
    -- *** Other
  , module Control.Applicative
  , module Data.Bits
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
import GHC.Stack
import GHC.TypeLits
import GHC.TypeLits.Extra
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
import CLaSH.Explicit.Mealy
import CLaSH.Explicit.Moore
import CLaSH.Explicit.RAM
import CLaSH.Explicit.ROM
import CLaSH.Explicit.Signal
import CLaSH.Explicit.Signal.Delayed
import CLaSH.Explicit.Synchronizer
  (dualFlipFlopSynchronizer, asyncFIFOSynchronizer)
import CLaSH.Prelude.BitIndex
import CLaSH.Prelude.BitReduction
import CLaSH.Prelude.DataFlow
import CLaSH.Prelude.ROM             (asyncRom, asyncRomPow2)
import CLaSH.Promoted.Nat
import CLaSH.Promoted.Nat.TH
import CLaSH.Promoted.Nat.Literals
import CLaSH.Sized.BitVector
import CLaSH.Sized.Fixed
import CLaSH.Sized.Index
import CLaSH.Sized.RTree
import CLaSH.Sized.Signed
import CLaSH.Sized.Unsigned
import CLaSH.Sized.Vector
import CLaSH.XException

{- $setup
>>> :set -XDataKinds
>>> import CLaSH.Explicit.Prelude
>>> let rP clk rst = registerB clk rst (8::Int,8::Int)
-}


-- | Create a 'register' function for product-type like signals (e.g.
-- @('Signal' a, 'Signal' b)@)
--
-- @
-- rP :: Clock domain gated -> Reset domain synchronous
--    -> ('Signal' domain Int, 'Signal' domain Int)
--    -> ('Signal' domain Int, 'Signal' domain Int)
-- rP clk rst = 'registerB' clk rst (8,8)
-- @
--
-- >>> simulateB (rP (systemClock (pure True)) systemReset) [(1,1),(2,2),(3,3)] :: [(Int,Int)]
-- [(8,8),(1,1),(2,2),(3,3)...
-- ...
registerB
  :: Bundle a
  => Clock domain gated
  -> Reset domain synchronous
  -> a
  -> Unbundled domain a
  -> Unbundled domain a
registerB clk rst i = unbundle Prelude.. register clk rst i Prelude.. bundle
{-# INLINE registerB #-}

-- | Give a pulse when the 'Signal'' goes from 'minBound' to 'maxBound'
isRising
  :: (Bounded a, Eq a)
  => Clock domain gated
  -> Reset domain synchronous
  -> a -- ^ Starting value
  -> Signal domain a
  -> Signal domain Bool
isRising clk rst is s = liftA2 edgeDetect prev s
  where
    prev = register clk rst is s
    edgeDetect old new = old == minBound && new == maxBound
{-# INLINABLE isRising #-}

-- | Give a pulse when the 'Signal'' goes from 'maxBound' to 'minBound'
isFalling
  :: (Bounded a, Eq a)
  => Clock domain gated
  -> Reset domain synchronous
  -> a -- ^ Starting value
  -> Signal domain a
  -> Signal domain Bool
isFalling clk rst is s = liftA2 edgeDetect prev s
  where
    prev = register clk rst is s
    edgeDetect old new = old == maxBound && new == minBound
{-# INLINABLE isFalling #-}

-- | Give a pulse every @n@ clock cycles. This is a useful helper function when
-- combined with functions like @'CLaSH.Signal.regEn'@ or @'CLaSH.Signal.mux'@,
-- in order to delay a register by a known amount.
riseEvery
  :: forall domain gated synchronous n
   . Clock domain gated
  -> Reset domain synchronous
  -> SNat n
  -> Signal domain Bool
riseEvery clk rst SNat = moore clk rst transfer output 0 (pure ())
  where
    output :: Index n -> Bool
    output = (== maxBound)

    transfer :: Index n -> () -> Index n
    transfer s _ = if (s == maxBound) then 0 else s+1
{-# INLINEABLE riseEvery #-}

-- | Oscillate a @'Bool'@ for a given number of cycles, given the starting state.
oscillate
  :: forall domain gated synchronous n
   . Clock domain gated
  -> Reset domain synchronous
  -> Bool
  -> SNat n
  -> Signal domain Bool
oscillate clk rst begin SNat = moore clk rst transfer snd (0, begin) (pure ())
  where
    transfer :: (Index n, Bool) -> () -> (Index n, Bool)
    transfer (s, i) _ =
      if s == maxBound
        then (0,   not i) -- reset state and oscillate output
        else (s+1, i)     -- hold current output
{-# INLINEABLE oscillate #-}

undefined :: HasCallStack => a
undefined = errorX "undefined"
