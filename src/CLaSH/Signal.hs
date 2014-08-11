module CLaSH.Signal
  ( -- * Implicitly clocked synchronous signal
    Signal
    -- * Basic circuit functions
  , signal
  , register
    -- * Product/Signal isomorphism
  , W.Wrap
  , Wrapped
  , wrap
  , unwrap
    -- * Simulation functions (not synthesisable)
  , simulate
  , simulateP
    -- * List \<-\> Signal conversion (not synthesisable)
  , sample
  , sampleN
  , fromList
  )
where

import CLaSH.Signal.Internal       (Signal, SystemClock, csignal)
import CLaSH.Signal.Explicit       (cfromList, cregister, csample, csampleN, systemClock)
import qualified CLaSH.Signal.Wrap as W

-- * Basic circuit functions

-- | Create a constant 'Signal' from a combinational value
--
-- >>> sample (signal 4)
-- [4, 4, 4, 4, ...
signal :: a -> Signal a
signal = csignal

-- | 'register' @i s@ delays the values in 'Signal' @s@ for one cycle, and sets
-- the value at time 0 to @i@
--
-- >>> sampleN 3 (register 8 (fromList [1,2,3,4]))
-- [8,1,2]
register :: a -> Signal a -> Signal a
register = cregister systemClock

-- * Product/Signal isomorphism

-- | Isomorphism between a 'Signal' of a product type (e.g. a tuple) and a
-- product type of 'Signal's.
type Wrapped a = W.Wrapped SystemClock a

wrap :: W.Wrap a => Signal a -> Wrapped a
wrap = W.wrap systemClock

unwrap :: W.Wrap a => Wrapped a -> Signal a
unwrap = W.unwrap systemClock

-- * Simulation functions (not synthesisable)

-- | Simulate a (@'Signal' a -> 'Signal' b@) function given a list of samples of
-- type @a@
--
-- >>> simulate (register 8) [1, 2, 3, ...
-- [8, 1, 2, 3, ...
simulate :: (Signal a -> Signal b) -> [a] -> [b]
simulate f = sample . f . fromList

-- | Simulate a (@'Wrapped' a -> 'Wrapped' b@) function given a list of samples
-- of type @a@
--
-- >>> simulateP (wrap . register (8,8) . unwrap) [(1,1), (2,2), (3,3), ...
-- [(8,8), (1,1), (2,2), (3,3), ...
simulateP :: (W.Wrap a, W.Wrap b) => (Wrapped a -> Wrapped b) -> [a] -> [b]
simulateP f = simulate (unwrap . f . wrap)

-- * List \<-\> Signal conversion (not synthesisable)

-- | Get an infinite list of samples from a 'Signal'
--
-- The elements in the list correspond to the values of the 'Signal' at
-- consecutive clock cycles
--
-- > sample s == [s0, s1, s2, s3, ...
sample :: Signal a -> [a]
sample = csample

-- | Get a list of @n@ samples from a 'Signal'
--
-- The elements in the list correspond to the values of the 'Signal' at
-- consecutive clock cycles
--
-- > sampleN 3 s == [s0, s1, s2]
sampleN :: Int -> Signal a -> [a]
sampleN = csampleN

-- | Create a 'Signal' from a list
--
-- Every element in the list will correspond to a value of the signal for one
-- clock cycle.
--
-- NB: Simulation only!
--
-- >>> sampleN 2 (fromList [1,2,3,4,5])
-- [1,2]
fromList :: [a] -> Signal a
fromList = cfromList
