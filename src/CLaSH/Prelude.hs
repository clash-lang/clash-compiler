{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

{- |
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
  library. A tutorial module will be added within due time.
-}
module CLaSH.Prelude
  ( -- * Creating synchronous sequential circuits
    (<^>)
  , registerP
    -- * BlockRAM primitives
  , blockRam
  , blockRamPow2
    -- * Utility functions
  , window
  , windowD
    -- * Testbench functions
  , sassert
  , stimuliGenerator
  , outputVerifier
    -- * Exported modules
    -- ** Synchronous signals
  , module CLaSH.Signal
    -- ** Datatypes
    -- *** Bit vectors
  , module CLaSH.Sized.BitVector
  , module CLaSH.Prelude.BitIndex
  , module CLaSH.Prelude.BitReduction
    -- *** Arbitrary-width numbers
  , module CLaSH.Sized.Signed
  , module CLaSH.Sized.Unsigned
    -- *** Fixed point numbers
  , module CLaSH.Sized.Fixed
    -- *** Fixed size vectors
  , module CLaSH.Sized.Vector
    -- ** Type-level natural numbers
  , module GHC.TypeLits
  , module CLaSH.Promoted.Nat
  , module CLaSH.Promoted.Nat.Literals
  , module CLaSH.Promoted.Nat.TH
    -- ** Type-level functions
  , module CLaSH.Promoted.Ord
    -- ** Template Haskell
  , Lift (..), deriveLift
    -- ** Type classes
    -- *** CLaSH
  , module CLaSH.Class.BitConvert
  , module CLaSH.Class.Num
  , module CLaSH.Class.Resize
    -- *** Other
  , module Control.Applicative
  , module Data.Bits
  , module Data.Default
  , module Prelude
  )
where

import Control.Applicative
import Data.Bits
import Data.Default
import GHC.TypeLits
import Language.Haskell.TH.Lift    (Lift(..),deriveLift)
import Prelude                     hiding ((++), (!!), concat, drop, foldl,
                                           foldl1, foldr, foldr1, head, init,
                                           iterate, last, length, map, repeat,
                                           replicate, reverse, scanl, scanl1,
                                           scanr, scanr1, splitAt, tail, take,
                                           unzip, zip, zipWith)

import CLaSH.Class.BitConvert
import CLaSH.Class.Num
import CLaSH.Class.Resize
import CLaSH.Prelude.BitIndex
import CLaSH.Prelude.BitReduction
import CLaSH.Prelude.BlockRam      (blockRam, blockRamPow2)
import CLaSH.Prelude.Explicit      (cregisterP, cwindow, cwindowD)
import CLaSH.Prelude.Mealy         ((<^>))
import CLaSH.Prelude.Testbench     (sassert, stimuliGenerator, outputVerifier)
import CLaSH.Promoted.Nat
import CLaSH.Promoted.Nat.TH
import CLaSH.Promoted.Nat.Literals
import CLaSH.Promoted.Ord
import CLaSH.Sized.BitVector
import CLaSH.Sized.Fixed
import CLaSH.Sized.Signed
import CLaSH.Sized.Unsigned
import CLaSH.Sized.Vector
import CLaSH.Signal
import CLaSH.Signal.Explicit       (systemClock)

{-# INLINE window #-}
-- | Give a window over a 'Signal'
--
-- > window4 :: Signal Int -> Vec 4 (Signal Int)
-- > window4 = window
--
-- >>> simulateP window4 [1,2,3,4,5,...
-- [<1,0,0,0>, <2,1,0,0>, <3,2,1,0>, <4,3,2,1>, <5,4,3,2>,...
window :: (KnownNat (n + 1), Default a)
       => Signal a                     -- ^ Signal to create a window over
       -> Vec ((n + 1) + 1) (Signal a) -- ^ Window of at least size 2
window = cwindow systemClock

{-# INLINE windowD #-}
-- | Give a delayed window over a 'Signal'
--
-- > windowD3 :: Signal Int -> Vec 3 (Signal Int)
-- > windowD3 = windowD
--
-- >>> simulateP windowD3 [1,2,3,4,...
-- [<0,0,0>, <1,0,0>, <2,1,0>, <3,2,1>, <4,3,2>,...
windowD :: (KnownNat (n + 1), Default a)
        => Signal a               -- ^ Signal to create a window over
        -> Vec (n + 1) (Signal a) -- ^ Window of at least size 1
windowD = cwindowD systemClock

{-# INLINE registerP #-}
-- | Create a 'register' function for product-type like signals (e.g. '(Signal a, Signal b)')
--
-- > rP :: (Signal Int,Signal Int) -> (Signal Int, Signal Int)
-- > rP = registerP (8,8)
--
-- >>> simulateP rP [(1,1),(2,2),(3,3),...
-- [(8,8),(1,1),(2,2),(3,3),...
registerP :: Wrap a => a -> SWrapped a -> SWrapped a
registerP = cregisterP systemClock
