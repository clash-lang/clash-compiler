{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE MagicHash #-}

module CLaSH.Signal.Explicit
  ( -- * Explicitly clocked synchronous signal
    -- $relativeclocks
    CSignal
    -- * Clock domain crossing
    -- ** Clock
  , Clock (..)
  , SClock (..)
  , sclock
  , SystemClock
  , systemClock
    -- ** Synchronisation primitive
  , veryUnsafeSynchronizer
    -- * Basic circuit functions
  , csignal
  , cregister
    -- * Product/Signal isomorphism
  , Wrap (..)
    -- * Simulation functions (not synthesisable)
  , csimulate
  , csimulateP
    -- * List \<-\> CSignal conversion (not synthesisable)
  , csample
  , csampleN
  , cfromList
  )
where

import qualified Data.Foldable as F
import GHC.TypeLits            (KnownNat, KnownSymbol)

import CLaSH.Promoted.Nat      (snat, snatToInteger)
import CLaSH.Promoted.Symbol   (ssymbol)
import CLaSH.Signal.Internal   (CSignal (..), Clock (..), SClock (..), signal#,
                                register#)
import CLaSH.Signal.Wrap       (Wrap (..), Wrapped)

{- $relativeclocks #relativeclocks#
CλaSH supports explicitly clocked 'CLaSH.Signal's in the form of:

> 'CSignal' (clk :: Clock) a

Where @a@ is the type of the elements, and @clk@ is the clock to which the
signal is synchronised. The type-parameter, @clk@, is of the kind 'Clock' which
has types of the following shape:

@
Clk \{\- name :: \-\} 'Symbol' \{\- period :: \-\} 'Nat'
@

Where @name@ is a type-level string ('Symbol') representing the the name of the
clock, and @period@ is a type-level natural number ('Nat') representing the
clock period. Two concrete instances of a 'Clk' could be:

> type ClkA500  = Clk "A500" 500
> type ClkB3250 = Clk "B3250" 3250

The periods of these clocks are however dimension-less, they do not refer to any
explicit time-scale (e.g. nano-seconds). The reason for the lack of an explicit
time-scale is that the CλaSH compiler would not be able guarantee that the
circuit can run at the specified frequency. The clock periods are just there to
indicate relative frequency differences between two different clocks. That is, a
signal:

> 'CSignal' ClkA500 a

is synchronized to a clock that runs 6.5 times faster than the clock to which
the signal:

> 'CSignal' ClkB3250 a

is synchronized to.

* __NB__: \"Bad things\"™  happen when you actually use a clock period of @0@,
so do __not__ do that!
* __NB__: You should be judicious using a clock with period of @1@ as you can
never create a clock that faster!
-}

-- * Clock domain crossing

-- ** Clock

-- | Create a singleton clock
sclock :: (KnownNat period, KnownSymbol name)
       => SClock (Clk name period)
sclock = SClock ssymbol snat

-- | The standard system clock with a period of 1000
type SystemClock = Clk "system" 1000

-- | The singleton clock for 'SystemClock'
systemClock :: SClock SystemClock
systemClock = sclock

-- ** Synchronisation primitive
{-# NOINLINE veryUnsafeSynchronizer #-}
-- | The 'veryUnsafeSynchronizer' function is a primitive that must be used to
-- connect one clock domain to the other, and will be synthesised to a (bundle
-- of) wire(s) in the eventual circuit. This function should only be used as
-- part of a proper synchronisation component, such as the following dual
-- flip-flop synchronizer:
--
-- > dualFlipFlop :: SClock clkA -> SClock clkB
-- >              -> CSignal clkA Bit -> CSignal clkB Bit
-- > dualFlipFlop clkA clkB = cregister clkB low . cregister clkB low
-- >                        . veryUnsafeSynchronizer clkA clkB
--
-- The 'veryUnsafeSynchronizer' works in such a way that, given 2 clocks:
--
-- > type Clk7 = Clk "clk7" 7
-- >
-- > clk7 :: SClock Clk7
-- > clk7 = sclock
--
-- and
--
-- > type Clk2 = Clk "clk2" 2
-- >
-- > clk2 :: SClock Clk2
-- > clk2 = sclock
--
-- Oversampling followed by compression is the identity function plus 2 initial
-- values:
--
-- > cregister clk7 i $
-- > veryUnsafeSynchronizer clk2 clk7 $
-- > cregister clk2 j $
-- > veryUnsafeSynchronizer clk7 clk2 $
-- > cregister clk7 k s
-- >
-- > ==
-- >
-- > i :- j :- s
--
-- Something we can easily observe:
--
-- > oversampling = cregister clk2 99 . veryUnsafeSynchronizer clk7 clk2
-- >              . cregister clk7 50
-- > almostId     = cregister clk7 70 . veryUnsafeSynchronizer clk2 clk7
-- >              . cregister clk2 99 . veryUnsafeSynchronizer clk7 clk2
-- >              . cregister clk7 50
--
-- >>> csample (oversampling (cfromList [1..10]))
-- [99, 50,1,1,1,2,2,2,2, 3,3,3,4,4,4,4, 5,5,5,6,6,6,6, 7,7,7,8,8,8,8, 9,9,9,10,10,10,10, ...
-- >>> csample (almostId (cfromList [1..10]))
-- [70, 99,1,2,3,4,5,6,7,8,9,10,...
veryUnsafeSynchronizer :: SClock clk1 -- ^ 'Clock' of the incoming signal
                       -> SClock clk2 -- ^ 'Clock' of the outgoing signal
                       -> CSignal clk1 a
                       -> CSignal clk2 a
veryUnsafeSynchronizer (SClock _ period1) (SClock _ period2) s = s'
  where
    t1    = fromInteger (snatToInteger period1)
    t2    = fromInteger (snatToInteger period2)
    s' | t1 < t2   = compress   t2 t1 s
       | t1 > t2   = oversample t1 t2 s
       | otherwise = same s

same :: CSignal clk1 a -> CSignal clk2 a
same (s :- ss) = s :- same ss

oversample :: Int -> Int -> CSignal clk1 a -> CSignal clk2 a
oversample high low (s :- ss) = s :- oversampleS (reverse (repSchedule high low)) ss

oversampleS :: [Int] -> CSignal clk1 a -> CSignal clk2 a
oversampleS sched = oversample' sched
  where
    oversample' []     s       = oversampleS sched s
    oversample' (d:ds) (s:-ss) = prefixN d s (oversample' ds ss)

    prefixN 0 _ s = s
    prefixN n x s = x :- prefixN (n-1) x s

compress :: Int -> Int -> CSignal clk1 a -> CSignal clk2 a
compress high low s = compressS (repSchedule high low) s

compressS :: [Int] -> CSignal clk1 a -> CSignal clk2 a
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

-- | Create a constant 'CSignal' from a combinational value
--
-- >>> csample (csignal 4)
-- [4, 4, 4, 4, ...
csignal :: a -> CSignal clk a
csignal = signal#

-- | \"@'cregister' i s@\" delays the values in 'CSignal' @s@ for one cycle,
-- and sets the value at time 0 to @i@
--
-- > type ClkA = Clk "A" 100
-- >
-- > clkA100 :: SClock ClkA
-- > clkA100 = sclock
--
-- >>> csampleN 3 (cregister clkA100 8 (fromList [1,2,3,4]))
-- [8,1,2]
cregister :: SClock clk -> a -> CSignal clk a -> CSignal clk a
cregister = register#

-- * Simulation functions

-- | Simulate a (@'CSignal' clk1 a -> 'CSignal' clk2 b@) function given a list
-- of samples of type @a@
--
-- > type ClkA = Clk "A" 100
-- >
-- > clkA100 :: SClock ClkA
-- > clkA100 = sclock
--
-- >>> csimulate (cregister clkA100 8) [1, 2, 3, ...
-- [8, 1, 2, 3, ...
--
-- __NB__: This function is not synthesisable
csimulate :: (CSignal clk1 a -> CSignal clk2 b) -> [a] -> [b]
csimulate f = csample . f . cfromList

-- | Simulate a (@'CSignalP' clk1 a -> 'CSignalP' clk2 b@) function given a list
-- of samples of type @a@
--
-- > type ClkA = Clk "A" 100
-- >
-- > clkA100 :: SClock ClkA
-- > clkA100 = sclock
--
-- >>> csimulateP clkA100 clkA100 (cunpack clkA100 . cregister clkA100 (8,8) . cpack clkA100) [(1,1), (2,2), (3,3), ...
-- [(8,8), (1,1), (2,2), (3,3), ...
--
-- __NB__: This function is not synthesisable
csimulateP :: (Wrap a, Wrap b)
           => SClock clk1 -- ^ 'Clock' of the incoming signal
           -> SClock clk2 -- ^ 'Clock' of the outgoing signal
           -> (Wrapped clk1 a -> Wrapped clk2 b) -- ^ Function to simulate
           -> [a] -> [b]
csimulateP clk1 clk2 f = csimulate (unwrap clk2 . f . wrap clk1)

-- * List \<-\> CSignal conversion

-- | Get an infinite list of samples from a 'CSignal'
--
-- The elements in the list correspond to the values of the 'CSignal' at
-- consecutive clock cycles
--
-- > csample s == [s0, s1, s2, s3, ...
--
-- __NB__: This function is not synthesisable
csample :: CSignal clk a -> [a]
csample = F.foldr (:) []

-- | Get a list of @n@ samples from a 'CSignal'
--
-- The elements in the list correspond to the values of the 'CSignal' at
-- consecutive clock cycles
--
-- > csampleN 3 s == [s0, s1, s2]
--
-- __NB__: This function is not synthesisable
csampleN :: Int -> CSignal clk a -> [a]
csampleN n = take n . csample

-- | Create a 'CSignal' from a list
--
-- Every element in the list will correspond to a value of the signal for one
-- clock cycle.
--
-- >>> csampleN 2 (cfromList [1,2,3,4,5])
-- [1,2]
--
-- __NB__: This function is not synthesisable
cfromList :: [a] -> CSignal clk a
cfromList = foldr (:-) (error "finite list")
