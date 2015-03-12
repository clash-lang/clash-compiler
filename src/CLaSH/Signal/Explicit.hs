{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE MagicHash #-}

module CLaSH.Signal.Explicit
  ( -- * Explicitly clocked synchronous signal
    -- $relativeclocks
    Signal'
    -- * Clock domain crossing
    -- ** Clock
  , Clock (..)
  , SClock (..)
  , sclock
  , withSClock
  , SystemClock
  , systemClock
    -- ** Synchronisation primitive
  , unsafeSynchronizer
    -- * Basic circuit functions
  , register'
  , regEn'
    -- * Product/Signal isomorphism
  , Bundle (..)
    -- * Simulation functions (not synthesisable)
  , simulateB'
  )
where

import GHC.TypeLits           (KnownNat, KnownSymbol)

import CLaSH.Promoted.Nat     (snat, snatToInteger)
import CLaSH.Promoted.Symbol  (ssymbol)
import CLaSH.Signal.Internal  (Signal' (..), Clock (..), SClock (..), register#,
                               regEn#, simulate)
import CLaSH.Signal.Bundle    (Bundle (..), Unbundled')

{- $relativeclocks #relativeclocks#
CλaSH supports explicitly clocked 'CLaSH.Signal's in the form of:

@
'Signal'' (clk :: 'Clock') a
@

Where @a@ is the type of the elements, and @clk@ is the clock to which the
signal is synchronised. The type-parameter, @clk@, is of the kind 'Clock' which
has types of the following shape:

@
Clk \{\- name :: \-\} 'GHC.TypeLits.Symbol' \{\- period :: \-\} 'GHC.TypeLits.Nat'
@

Where @name@ is a type-level string ('GHC.TypeLits.Symbol') representing the the
name of the clock, and @period@ is a type-level natural number ('GHC.TypeLits.Nat')
representing the clock period. Two concrete instances of a 'Clk' could be:

> type ClkA500  = Clk "A500" 500
> type ClkB3250 = Clk "B3250" 3250

The periods of these clocks are however dimension-less, they do not refer to any
explicit time-scale (e.g. nano-seconds). The reason for the lack of an explicit
time-scale is that the CλaSH compiler would not be able guarantee that the
circuit can run at the specified frequency. The clock periods are just there to
indicate relative frequency differences between two different clocks. That is, a
signal:

@
'Signal'' ClkA500 a
@

is synchronized to a clock that runs 6.5 times faster than the clock to which
the signal:

@
'Signal'' ClkB3250 a
@

is synchronized to.

* __NB__: \"Bad things\"™  happen when you actually use a clock period of @0@,
so do __not__ do that!
* __NB__: You should be judicious using a clock with period of @1@ as you can
never create a clock that faster!
-}

-- * Clock domain crossing

-- ** Clock

{-# INLINE sclock #-}
-- | Create a singleton clock
--
-- @
-- type ClkA = Clk \"A\" 100
--
-- clkA :: SClock ClkA
-- clkA = 'sclock'
-- @
sclock :: (KnownSymbol name, KnownNat period)
       => SClock (Clk name period)
sclock = SClock ssymbol snat

{-# INLINE withSClock #-}
withSClock :: (KnownSymbol name, KnownNat period)
           => (SClock (Clk name period) -> a)
           -> a
withSClock f = f (SClock ssymbol snat)

-- | The standard system clock with a period of 1000
type SystemClock = Clk "system" 1000

{-# INLINE systemClock #-}
-- | The singleton clock for 'SystemClock'
systemClock :: SClock SystemClock
systemClock = sclock

-- ** Synchronisation primitive
{-# NOINLINE unsafeSynchronizer #-}
-- | The 'unsafeSynchronizer' function is a primitive that must be used to
-- connect one clock domain to the other, and will be synthesised to a (bundle
-- of) wire(s) in the eventual circuit. This function should only be used as
-- part of a proper synchronisation component, such as the following dual
-- flip-flop synchronizer:
--
-- @
-- dualFlipFlop :: SClock clkA -> SClock clkB
--              -> Signal' clkA Bit -> Signal' clkB Bit
-- dualFlipFlop clkA clkB = 'register'' clkB low . 'register'' clkB low
--                        . 'unsafeSynchronizer' clkA clkB
-- @
--
-- The 'unsafeSynchronizer' works in such a way that, given 2 clocks:
--
-- @
-- type Clk7 = Clk \"clk7\" 7
--
-- clk7 :: SClock Clk7
-- clk7 = 'sclock'
-- @
--
-- and
--
-- @
-- type Clk2 = Clk \"clk2\" 2
--
-- clk2 :: SClock Clk2
-- clk2 = 'sclock'
-- @
--
-- Oversampling followed by compression is the identity function plus 2 initial
-- values:
--
-- @
-- 'register'' clk7 i $
-- 'unsafeSynchronizer' clk2 clk7 $
-- 'register'' clk2 j $
-- 'unsafeSynchronizer' clk7 clk2 $
-- 'register'' clk7 k s
--
-- ==
--
-- i :- j :- s
-- @
--
-- Something we can easily observe:
--
-- @
-- oversampling = 'register'' clk2 99 . 'unsafeSynchronizer' clk7 clk2
--              . 'register'' clk7 50
-- almostId     = 'register'' clk7 70 . 'unsafeSynchronizer' clk2 clk7
--              . 'register'' clk2 99 . 'unsafeSynchronizer' clk7 clk2
--              . 'register'' clk7 50
-- @
--
-- >>> sample (oversampling (fromList [1..10]))
-- [99, 50,1,1,1,2,2,2,2, 3,3,3,4,4,4,4, 5,5,5,6,6,6,6, 7,7,7,8,8,8,8, 9,9,9,10,10,10,10, ...
-- >>> sample (almostId (fromList [1..10]))
-- [70, 99,1,2,3,4,5,6,7,8,9,10,...
unsafeSynchronizer :: SClock clk1 -- ^ 'Clock' of the incoming signal
                   -> SClock clk2 -- ^ 'Clock' of the outgoing signal
                   -> Signal' clk1 a
                   -> Signal' clk2 a
unsafeSynchronizer (SClock _ period1) (SClock _ period2) s = s'
  where
    t1    = fromInteger (snatToInteger period1)
    t2    = fromInteger (snatToInteger period2)
    s' | t1 < t2   = compress   t2 t1 s
       | t1 > t2   = oversample t1 t2 s
       | otherwise = same s

same :: Signal' clk1 a -> Signal' clk2 a
same (s :- ss) = s :- same ss

oversample :: Int -> Int -> Signal' clk1 a -> Signal' clk2 a
oversample high low (s :- ss) = s :- oversampleS (reverse (repSchedule high low)) ss

oversampleS :: [Int] -> Signal' clk1 a -> Signal' clk2 a
oversampleS sched = oversample' sched
  where
    oversample' []     s       = oversampleS sched s
    oversample' (d:ds) (s:-ss) = prefixN d s (oversample' ds ss)

    prefixN 0 _ s = s
    prefixN n x s = x :- prefixN (n-1) x s

compress :: Int -> Int -> Signal' clk1 a -> Signal' clk2 a
compress high low s = compressS (repSchedule high low) s

compressS :: [Int] -> Signal' clk1 a -> Signal' clk2 a
compressS sched = compress' sched
  where
    compress' []     s           = compressS sched s
    compress' (d:ds) ss@(s :- _) = s :- compress' ds (dropS d ss)

    dropS 0 s         = s
    dropS n (_ :- ss) = dropS (n-1) ss

repSchedule :: Int -> Int -> [Int]
repSchedule high low = take low $ repSchedule' low high 1
  where
    repSchedule' cnt th rep
      | cnt < th  = repSchedule' (cnt+low) th (rep + 1)
      | otherwise = rep : repSchedule' (cnt + low) (th + high) 1

-- * Basic circuit functions

{-# INLINE register' #-}
-- | \"@'register'' i s@\" delays the values in 'Signal'' @s@ for one cycle,
-- and sets the value at time 0 to @i@
--
-- @
-- type ClkA = Clk \"A\" 100
--
-- clkA :: SClock ClkA
-- clkA = 'sclock'
-- @
--
-- >>> sampleN 3 (register' clkA 8 (fromList' [1,2,3,4]))
-- [8,1,2]
register' :: SClock clk -> a -> Signal' clk a -> Signal' clk a
register' = register#

{-# INLINE regEn' #-}
-- | Version of 'register'' that only updates its content when its second
-- argument is asserted. So given:
--
-- @
-- type ClkA = Clk \"A\" 100
-- clkA :: SClock ClkA
-- clkA = 'sclock'
--
-- oscillate = 'register'' clkA False ('CLaSH.Signal.not1' oscillate)
-- count     = 'regEn'' clkA 0 oscillate (count + 1)
-- @
--
-- We get:
--
-- >>> sampleN 8 oscillate
-- [False,True,False,True,False,True,False,True]
-- >>> sampleN 8 count
-- [0,0,1,1,2,2,3,3]
regEn' :: SClock clk -> a -> Signal' clk Bool -> Signal' clk a -> Signal' clk a
regEn' = regEn#

-- * Simulation functions

-- | Simulate a (@'Unbundled'' clk1 a -> 'Unbundled'' clk2 b@) function given a
-- list of samples of type @a@
--
-- @
-- type ClkA = Clk \"A\" 100
--
-- clkA :: SClock ClkA
-- clkA = 'sclock'
-- @
--
-- >>> simulateB' clkA clkA (unbundle' clkA . register' clkA (8,8) . bundle' clkA) [(1,1), (2,2), (3,3)] :: [(Int,Int)]
-- [(8,8), (1,1), (2,2), (3,3), *** Exception: finite list
--
-- __NB__: This function is not synthesisable
simulateB' :: (Bundle a, Bundle b)
           => SClock clk1 -- ^ 'Clock' of the incoming signal
           -> SClock clk2 -- ^ 'Clock' of the outgoing signal
           -> (Unbundled' clk1 a -> Unbundled' clk2 b) -- ^ Function to simulate
           -> [a] -> [b]
simulateB' clk1 clk2 f = simulate (bundle' clk2 . f . unbundle' clk1)
