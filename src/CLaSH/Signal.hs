{-# LANGUAGE MagicHash #-}

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
  , Unbundled'
  , bundle'
  , unbundle'
    -- * Simulation functions (not synthesisable)
  , simulate
  , simulateB
    -- * List \<-\> Signal conversion (not synthesisable)
  , sample
  , sampleN
  , fromList
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

import CLaSH.Signal.Internal  (CSignal, register#, regEn#, signal#, (.==.), (./=.),
                               compare1, (.<.), (.<=.), (.>=.), (.>.), fromEnum1,
                               toRational1, toInteger1, testBit1, popCount1,
                               shift1, rotate1, setBit1, clearBit1, shiftL1,
                               unsafeShiftL1, shiftR1, unsafeShiftR1, rotateL1,
                               rotateR1, (.||.), (.&&.), not1, mux)
import CLaSH.Signal.Explicit  (SystemClock, cfromList, csample, csampleN,
                               systemClock)
import CLaSH.Signal.Bundle    (Bundle (..), Unbundled)

-- * Implicitly clocked synchronous signal

-- | Signal synchronised to the \"system\" clock, which has a period of 1000.
type Signal a = CSignal SystemClock a

-- * Basic circuit functions

{-# INLINE signal #-}
-- | Create a constant 'Signal' from a combinational value
--
-- >>> sample (signal 4)
-- [4, 4, 4, 4, ...
signal :: a -> Signal a
signal = signal#

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
-- oscillate = register False ('not1' oscillate)
-- count     = regEn 0 oscillate (count + 1)
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
type Unbundled' a = Unbundled SystemClock a

{-# INLINE unbundle' #-}
-- | Example:
--
-- > unbundle' :: Signal (a,b) -> (Signal a, Signal b)
--
-- However:
--
-- > unbundle' :: Signal Bit -> Signal Bit
unbundle' :: Bundle a => Signal a -> Unbundled' a
unbundle' = unbundle systemClock

{-# INLINE bundle' #-}
-- | Example:
--
-- > bundle' :: (Signal a, Signal b) -> Signal (a,b)
--
-- However:
--
-- > bundle' :: Signal Bit -> Signal Bit
bundle' :: Bundle a => Unbundled' a -> Signal a
bundle' = bundle systemClock

-- * Simulation functions (not synthesisable)

-- | Simulate a (@'Signal' a -> 'Signal' b@) function given a list of samples of
-- type @a@
--
-- >>> simulate (register 8) [1, 2, 3, ...
-- [8, 1, 2, 3, ...
--
-- __NB__: This function is not synthesisable
simulate :: (Signal a -> Signal b) -> [a] -> [b]
simulate f = sample . f . fromList

-- | Simulate a (@'Unbundled'' a -> 'Unbundled'' b@) function given a list of
-- samples of type @a@
--
-- >>> simulateB (unbundle' . register (8,8) . bundle') [(1,1), (2,2), (3,3)] :: [(Int,Int)]
-- [(8,8), (1,1), (2,2), (3,3),*** Exception: finite list
--
-- __NB__: This function is not synthesisable
simulateB :: (Bundle a, Bundle b) => (Unbundled' a -> Unbundled' b) -> [a] -> [b]
simulateB f = simulate (bundle' . f . unbundle')

-- * List \<-\> Signal conversion (not synthesisable)

-- | Get an infinite list of samples from a 'Signal'
--
-- The elements in the list correspond to the values of the 'Signal' at
-- consecutive clock cycles
--
-- > sample s == [s0, s1, s2, s3, ...
--
-- __NB__: This function is not synthesisable
sample :: Signal a -> [a]
sample = csample

-- | Get a list of @n@ samples from a 'Signal'
--
-- The elements in the list correspond to the values of the 'Signal' at
-- consecutive clock cycles
--
-- > sampleN 3 s == [s0, s1, s2]
--
-- __NB__: This function is not synthesisable
sampleN :: Int -> Signal a -> [a]
sampleN = csampleN

-- | Create a 'Signal' from a list
--
-- Every element in the list will correspond to a value of the signal for one
-- clock cycle.
--
-- >>> sampleN 2 (fromList [1,2,3,4,5])
-- [1,2]
--
-- __NB__: This function is not synthesisable
fromList :: [a] -> Signal a
fromList = cfromList
