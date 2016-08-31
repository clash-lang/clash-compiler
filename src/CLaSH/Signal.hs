{-|
Copyright  :  (C) 2013-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE MagicHash #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module CLaSH.Signal
  ( -- * Implicitly clocked synchronous signal
    Signal
    -- * Basic circuit functions
  , signal
  , register
  , regEn
  , mux
    -- * Boolean connectives
  , (.&&.), (.||.)
    -- * Product/Signal isomorphism
  , Bundle(..)
  , Unbundled
    -- * Simulation functions (not synthesisable)
  , simulate
  , simulateB
    -- ** lazy versions
  , simulate_lazy
  , simulateB_lazy
    -- * List \<-\> Signal conversion (not synthesisable)
  , sample
  , sampleN
  , fromList
    -- ** lazy versions
  , sample_lazy
  , sampleN_lazy
  , fromList_lazy
    -- * QuickCheck combinators
  , testFor
    -- * Type classes
    -- ** 'Eq'-like
  , (.==.), (./=.)
    -- ** 'Ord'-like
  , (.<.), (.<=.), (.>=.), (.>.)
  )
where

import Control.DeepSeq       (NFData)
import Data.Bits             (Bits) -- Haddock only

import CLaSH.Signal.Internal (Signal', register#, regEn#, (.==.), (./=.),
                              (.<.), (.<=.), (.>=.), (.>.), (.||.), (.&&.),
                              mux, sample,sampleN, fromList, simulate, signal,
                              testFor, sample_lazy, sampleN_lazy, simulate_lazy,
                              fromList_lazy)
import CLaSH.Signal.Explicit (SystemClock, systemClock)
import CLaSH.Signal.Bundle   (Bundle (..), Unbundled')

{- $setup
>>> let oscillate = register False (not <$> oscillate)
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
infixr 3 `register`

{-# INLINE regEn #-}
-- | Version of 'register' that only updates its content when its second argument
-- is asserted. So given:
--
-- @
-- oscillate = 'register' False ('not' <$> oscillate)
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

-- | Simulate a (@'Unbundled' a -> 'Unbundled' b@) function given a list of
-- samples of type @a@
--
-- >>> simulateB (unbundle . register (8,8) . bundle) [(1,1), (2,2), (3,3)] :: [(Int,Int)]
-- [(8,8),(1,1),(2,2),(3,3)...
-- ...
--
-- __NB__: This function is not synthesisable
simulateB :: (Bundle a, Bundle b, NFData a, NFData b) => (Unbundled' clk1 a -> Unbundled' clk2 b) -> [a] -> [b]
simulateB f = simulate (bundle . f . unbundle)

-- | Simulate a (@'Unbundled' a -> 'Unbundled' b@) function given a list of
-- samples of type @a@
--
-- >>> simulateB (unbundle . register (8,8) . bundle) [(1,1), (2,2), (3,3)] :: [(Int,Int)]
-- [(8,8),(1,1),(2,2),(3,3)...
-- ...
--
-- __NB__: This function is not synthesisable
simulateB_lazy :: (Bundle a, Bundle b) => (Unbundled' clk1 a -> Unbundled' clk2 b) -> [a] -> [b]
simulateB_lazy f = simulate_lazy (bundle . f . unbundle)
