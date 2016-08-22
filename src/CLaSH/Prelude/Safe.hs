{-|
  Copyright   :  (C) 2013-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  __This is the <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/safe-haskell.html Safe> API only of "CLaSH.Prelude"__

  CλaSH (pronounced ‘clash’) is a functional hardware description language that
  borrows both its syntax and semantics from the functional programming language
  Haskell. The merits of using a functional language to describe hardware comes
  from the fact that combinational circuits can be directly modeled as
  mathematical functions and that functional languages lend themselves very well
  at describing and (de-)composing mathematical functions.

  This package provides:

  * Prelude library containing datatypes and functions for circuit design

  To use the library:

  * Import "CLaSH.Prelude"
  * Additionally import "CLaSH.Prelude.Explicit" if you want to design
    explicitly clocked circuits in a multi-clock setting

  For now, "CLaSH.Prelude" is also the best starting point for exploring the
  library. A preliminary version of a tutorial can be found in "CLaSH.Tutorial".
  Some circuit examples can be found in "CLaSH.Examples".
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module CLaSH.Prelude.Safe
  ( -- * Creating synchronous sequential circuits
    mealy
  , mealyB
  , (<^>)
  , moore
  , mooreB
  , registerB
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
    -- * BlockRAM read/write conflict resolution
  , readNew
  , readNew'
    -- * Utility functions
  , isRising
  , isFalling
    -- * Exported modules
    -- ** Synchronous signals
  , module CLaSH.Signal
  , module CLaSH.Signal.Delayed
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
    -- ** Haskell Prelude
    -- $hiding
  , module Prelude
  )
where

import Control.Applicative
import Data.Bits
import GHC.TypeLits
import GHC.TypeLits.Extra
import Prelude                     hiding ((++), (!!), concat, drop, foldl,
                                           foldl1, foldr, foldr1, head, init,
                                           iterate, last, length, map, repeat,
                                           replicate, reverse, scanl, scanr,
                                           splitAt, tail, take, unzip, unzip3,
                                           zip, zip3, zipWith, zipWith3)

import CLaSH.Annotations.TopEntity
import CLaSH.Class.BitPack
import CLaSH.Class.Num
import CLaSH.Class.Resize
import CLaSH.Prelude.BitIndex
import CLaSH.Prelude.BitReduction
import CLaSH.Prelude.BlockRam      (blockRam, blockRamPow2, readNew, readNew')
import CLaSH.Prelude.Explicit.Safe (registerB', isRising', isFalling')
import CLaSH.Prelude.Mealy         (mealy, mealyB, (<^>))
import CLaSH.Prelude.Moore         (moore, mooreB)
import CLaSH.Prelude.RAM           (asyncRam,asyncRamPow2)
import CLaSH.Prelude.ROM           (asyncRom,asyncRomPow2,rom,romPow2)
import CLaSH.Prelude.DataFlow
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
import CLaSH.Signal
import CLaSH.Signal.Delayed
import CLaSH.Signal.Explicit       (systemClock)

{- $setup
>>> let rP = registerB (8,8)
-}

{- $hiding
"CLaSH.Prelude" re-exports most of the Haskell "Prelude" with the exception of
the following: (++), (!!), concat, drop, foldl, foldl1, foldr, foldr1, head,
init, iterate, last, length, map, repeat, replicate, reverse, scanl, scanr,
splitAt, tail, take, unzip, unzip3, zip, zip3, zipWith, zipWith3.

It instead exports the identically named functions defined in terms of
'CLaSH.Sized.Vector.Vec' at "CLaSH.Sized.Vector".
-}

{-# INLINE registerB #-}
-- | Create a 'register' function for product-type like signals (e.g. '(Signal a, Signal b)')
--
-- > rP :: (Signal Int,Signal Int) -> (Signal Int, Signal Int)
-- > rP = registerB (8,8)
--
-- >>> simulateB rP [(1,1),(2,2),(3,3)] :: [(Int,Int)]
-- [(8,8),(1,1),(2,2),(3,3)...
-- ...
registerB :: Bundle a => a -> Unbundled a -> Unbundled a
registerB = registerB' systemClock
infixr 3 `registerB`

{-# INLINE isRising #-}
-- | Give a pulse when the 'Signal' goes from 'minBound' to 'maxBound'
isRising :: (Bounded a, Eq a)
         => a -- ^ Starting value
         -> Signal a
         -> Signal Bool
isRising = isRising' systemClock

{-# INLINE isFalling #-}
-- | Give a pulse when the 'Signal' goes from 'maxBound' to 'minBound'
isFalling :: (Bounded a, Eq a)
          => a -- ^ Starting value
          -> Signal a
          -> Signal Bool
isFalling = isFalling' systemClock
