{-# LANGUAGE MagicHash #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Copyright  :  (C) 2013-2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
module CLaSH.Signal
  ( -- * Implicitly clocked synchronous signal
    Signal
    -- * Basic circuit functions
  , signal
  , register
  , regEn
  , mux
    -- * Boolean connectives
  , (.&&.), (.||.), not1
    -- * Product/Signal isomorphism
  , Bundle
  , Unbundled
  , bundle
  , unbundle
    -- * Simulation functions (not synthesisable)
  , simulate
  , simulateB
    -- * List \<-\> Signal conversion (not synthesisable)
  , sample
  , sampleN
  , fromList
    -- * QuickCheck combinators
  , testFor
    -- * Type classes
    -- ** 'Eq'-like
  , (.==.), (./=.)
    -- ** 'Ord'-like
  , compare1, (.<.), (.<=.), (.>=.), (.>.)
    -- ** 'Enum'-like
  , fromEnum1
    -- ** 'Rational'-like
  , toRational1
    -- ** 'Integral'-like
  , toInteger1
    -- ** 'Bits'-like
  , testBit1
  , popCount1
  , shift1
  , rotate1
  , setBit1
  , clearBit1
  , shiftL1
  , unsafeShiftL1
  , shiftR1
  , unsafeShiftR1
  , rotateL1
  , rotateR1
  )
where

import Data.Bits             (Bits) -- Haddock only

import CLaSH.Signal.Internal (Signal', register#, regEn#, (.==.), (./=.),
                              compare1, (.<.), (.<=.), (.>=.), (.>.), fromEnum1,
                              toRational1, toInteger1, testBit1, popCount1,
                              shift1, rotate1, setBit1, clearBit1, shiftL1,
                              unsafeShiftL1, shiftR1, unsafeShiftR1, rotateL1,
                              rotateR1, (.||.), (.&&.), not1, mux, sample,
                              sampleN, fromList, simulate, signal, testFor)
import CLaSH.Signal.Explicit (SystemClock, systemClock, simulateB')
import CLaSH.Signal.Bundle   (Bundle (..), Unbundled')

{- $setup
>>> let oscillate = register False (not1 oscillate)
>>> let count = regEn 0 oscillate (count + 1)
-}

-- * Implicitly clocked synchronous signal

-- | Signal synchronised to the \"system\" clock, which has a period of 1000.
type Signal a = Signal' SystemClock a

-- * Basic circuit functions

{-# INLINE register #-}
-- | 'register' @i s@ delays the values in 'Signal' @s@ for one cycle, and sets
-- the value at time 0 to @i@
--
-- >>> sampleN 3 (register 8 (fromList [1,2,3,4]))
-- [8,1,2]
register :: a -> Signal a -> Signal a
register = register# systemClock

{-# INLINE regEn #-}
-- | Version of 'register' that only updates its content when its second argument
-- is asserted. So given:
--
-- @
-- oscillate = 'register' False ('not1' oscillate)
-- count     = 'regEn' 0 oscillate (count + 1)
-- @
--
-- We get:
--
-- >>> sampleN 8 oscillate
-- [False,True,False,True,False,True,False,True]
-- >>> sampleN 8 count
-- [0,0,1,1,2,2,3,3]
regEn :: a -> Signal Bool -> Signal a -> Signal a
regEn = regEn# systemClock

-- * Product/Signal isomorphism

-- | Isomorphism between a 'Signal' of a product type (e.g. a tuple) and a
-- product type of 'Signal's.
type Unbundled a = Unbundled' SystemClock a

{-# INLINE unbundle #-}
-- | Example:
--
-- @
-- __unbundle__ :: 'Signal' (a,b) -> ('Signal' a, 'Signal' b)
-- @
--
-- However:
--
-- @
-- __unbundle__ :: 'Signal' 'CLaSH.Sized.BitVector.Bit' -> 'Signal' 'CLaSH.Sized.BitVector.Bit'
-- @
unbundle :: Bundle a => Signal a -> Unbundled a
unbundle = unbundle' systemClock

{-# INLINE bundle #-}
-- | Example:
--
-- @
-- __bundle__ :: ('Signal' a, 'Signal' b) -> 'Signal' (a,b)
-- @
--
-- However:
--
-- @
-- __bundle__ :: 'Signal' 'CLaSH.Sized.BitVector.Bit' -> 'Signal' 'CLaSH.Sized.BitVector.Bit'
-- @
bundle :: Bundle a => Unbundled a -> Signal a
bundle = bundle' systemClock

-- | Simulate a (@'Unbundled' a -> 'Unbundled' b@) function given a list of
-- samples of type @a@
--
-- >>> simulateB (unbundle . register (8,8) . bundle) [(1,1), (2,2), (3,3)] :: [(Int,Int)]
-- [(8,8),(1,1),(2,2),(3,3)...
--
-- __NB__: This function is not synthesisable
simulateB :: (Bundle a, Bundle b) => (Unbundled a -> Unbundled b) -> [a] -> [b]
simulateB = simulateB' systemClock systemClock
