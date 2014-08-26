{-# LANGUAGE MagicHash #-}

module CLaSH.Signal
  ( -- * Implicitly clocked synchronous signal
    Signal
    -- * Basic circuit functions
  , signal
  , register
    -- * Product/Signal isomorphism
  , Wrap
  , SWrapped
  , sUnwrap
  , sWrap
    -- * Simulation functions (not synthesisable)
  , simulate
  , simulateW
    -- * List \<-\> Signal conversion (not synthesisable)
  , sample
  , sampleN
  , fromList
  )
where

import CLaSH.Signal.Internal  (CSignal, register#, signal#)
import CLaSH.Signal.Explicit  (SystemClock, cfromList, csample, csampleN,
                               systemClock)
import CLaSH.Signal.Wrap      (Wrap (..), Wrapped)

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

-- * Product/Signal isomorphism

-- | Isomorphism between a 'Signal' of a product type (e.g. a tuple) and a
-- product type of 'Signal's.
type SWrapped a = Wrapped SystemClock a

{-# INLINE sWrap #-}
-- | Example:
--
-- > sWrap :: Signal (a,b) -> (Signal a, Signal b)
--
-- However:
--
-- > sWrap :: Signal Bit -> Signal Bit
sWrap :: Wrap a => Signal a -> SWrapped a
sWrap = wrap systemClock

{-# INLINE sUnwrap #-}
-- | Example:
--
-- > sUnwrap :: (Signal a, Signal b) -> Signal (a,b)
--
-- However:
--
-- > sUnwrap :: Signal Bit -> Signal Bit
sUnwrap :: Wrap a => SWrapped a -> Signal a
sUnwrap = unwrap systemClock

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

-- | Simulate a (@'Wrapped' a -> 'Wrapped' b@) function given a list of samples
-- of type @a@
--
-- >>> simulateW (wrap . register (8,8) . unwrap) [(1,1), (2,2), (3,3), ...
-- [(8,8), (1,1), (2,2), (3,3), ...
--
-- __NB__: This function is not synthesisable
simulateW :: (Wrap a, Wrap b) => (SWrapped a -> SWrapped b) -> [a] -> [b]
simulateW f = simulate (sUnwrap . f . sWrap)

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
