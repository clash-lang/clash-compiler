{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017     , Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE MagicHash #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module CLaSH.Explicit.Signal
  ( -- * Explicitly clocked synchronous signal
    -- $relativeclocks
    Signal, Domain (..), System
    -- * Clock
  , Clock (Clock) , ClockKind (..)
  , freqCalc
    -- ** Synchronisation primitive
  , unsafeSynchronizer
    -- ** Glock gating
  , clockGate
    -- * Reset
  , Reset (..), ResetKind (..)
  , unsafeFromAsyncReset
  , unsafeToAsyncReset
  , fromSyncReset
  , toSyncReset
  , resetSynchroniser
    -- * Basic circuit functions
  , delay
  , register
  , regMaybe
  , regEn
    -- * Testbench functions
  , clockGen
  , asyncResetGen
  , syncResetGen
    -- * Boolean connectives
  , (.&&.), (.||.)
    -- * Product/Signal isomorphism
  , Bundle(..)
    -- * Simulation functions (not synthesisable)
  , systemClock, systemReset
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
import Data.Maybe            (isJust, fromJust)
import GHC.Stack             (HasCallStack, withFrozenCallStack)

import CLaSH.Promoted.Nat    (snatToNum)
import CLaSH.Signal.Internal
import CLaSH.Signal.Bundle   (Bundle (..))

{- $setup
>>> :set -XDataKinds -XTypeApplications
>>> import CLaSH.Explicit.Prelude
>>> import qualified Data.List as L
>>> type Dom2 = Dom "dom" 2
>>> type Dom7 = Dom "dom" 7
>>> let clk2 = Clock @Dom2 (pure True)
>>> let clk7 = Clock @Dom7 (pure True)
>>> let oversampling clkA clkB = delay clkB . unsafeSynchronizer clkA clkB . delay clkA
>>> let almostId clkA clkB = delay clkB . unsafeSynchronizer clkA clkB . delay clkA . unsafeSynchronizer clkB clkA . delay clkB
>>> let oscillate clk rst = let s = register clk rst False (not <$> s) in s
>>> let count clk rst = let s = regEn clk rst 0 (oscillate clk rst) (s + 1) in s
>>> :{
sometimes1 clk rst = s where
  s = register clk rst Nothing (switch <$> s)
  switch Nothing = Just 1
  switch _       = Nothing
:}

>>> :{
countSometimes clk rst = s where
  s = regMaybe clk rst 0 (plusM (pure <$> s) (sometimes1 clk rst))
  plusM = liftA2 (liftA2 (+))
:}

-}

{- $relativeclocks #relativeclocks#
CλaSH supports explicitly clocked 'CLaSH.Signal's in the form of:

@
'Signal' (domain :: 'Domain') a
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
never create a clock that goes any faster!
-}

-- **Clock

-- | A clock domain running at 100 MHz
type System = 'Dom "system" 10000

-- | Clock generator for the 'System' clock domain.
--
-- __NB__: should only be used for simulation
systemClock
  :: Signal System Bool
  -> Clock System 'Source
systemClock = clockGen

-- | Reset generator for the 'System' clock domain.
--
-- __NB__: should only be used for simulation
systemReset :: Reset System 'Asynchronous
systemReset = asyncResetGen

resetSynchroniser
  :: Clock domain gated
  -> Reset domain 'Asynchronous
  -> Reset domain 'Asynchronous
resetSynchroniser clk rst  =
  let r1 = register clk rst True (pure False)
      r2 = register clk rst True r1
  in  unsafeToAsyncReset r2

-- | Calculate relative periods given a list of frequencies.
--
-- So for example, you have one part of your design connected to an ADC running
-- at 20 MHz, one part of your design connected to a DAC running at 36 MHz, and
-- the rest of your system is running at 50 MHz. What are the relative
-- (integer) clock periods in CλaSH, such that their ratios correspond to the
-- ratios between the actual clock frequencies.
--
-- For this we use 'freqCalc':
--
-- >>> freqCalc [20,36,50]
-- [45,25,18]
--
-- So that we create the proper clocks:
--
-- @
-- type ADC20 = 'Clk' \"ADC\" 45
-- type DAC36 = 'Clk' \"DAC\" 25
-- type Sys50 = 'Clk' \"Sys\" 18
--
-- sys50 :: SClock Sys50
-- sys50 = 'sclock'
--
-- adc20 :: SClock ADC20
-- adc20 = 'sclock'
--
-- dac36 :: SClock DAC36
-- dac36 = 'sclock'
-- @
--
-- __NB__: This function is /not/ synthesisable
freqCalc :: [Integer] -> [Integer]
freqCalc xs = map (`div` g) ys
  where
    p  = product xs
    ys = map (p `div`) xs
    g  = foldr1 gcd ys

-- ** Synchronisation primitive
{-# NOINLINE unsafeSynchronizer #-}
-- | The 'unsafeSynchronizer' function is a primitive that must be used to
-- connect one clock domain to the other, and will be synthesised to a (bundle
-- of) wire(s) in the eventual circuit. This function should only be used as
-- part of a proper synchronisation component, such as the following dual
-- flip-flop synchronizer:
--
-- @
-- dualFlipFlop :: Clock domA gatedA -> Clock domB gatedB
--              -> Signal domA Bit -> Signal domB Bit
-- dualFlipFlop clkA clkB = 'delay' clkB . 'delay' clkB
--                        . 'unsafeSynchronizer' clkA clkB
-- @
--
-- The 'unsafeSynchronizer' works in such a way that, given 2 clocks:
--
-- @
-- type Dom7 = 'Dom' \"dom\" 7
--
-- clk7 :: 'Clock' Dom7 Source
-- clk7 = 'Clock' (pure True)
-- @
--
-- and
--
-- @
-- type Dom2 = 'Dom' \"dom\" 2
--
-- clk2 :: 'Clock' Dom2 Source
-- clk2 = 'Clock' (pure True)
-- @
--
-- Oversampling followed by compression is the identity function plus 2 initial
-- values:
--
-- @
-- 'delay' clkB $
-- 'unsafeSynchronizer' clkA clkB $
-- 'delay' clkA $
-- 'unsafeSynchronizer' clkB clkA $
-- 'delay' clkB s
--
-- ==
--
-- X :- X :- s
-- @
--
-- Something we can easily observe:
--
-- @
-- oversampling clkA clkB = 'delay' clkB . 'unsafeSynchronizer' clkA clkB
--                        . 'delay' clkA
-- almostId clkA clkB = 'delay' clkB . 'unsafeSynchronizer' clkA clkB
--                    . 'delay' clkA . 'unsafeSynchronizer' clkB clkA
--                    . 'delay' clkB
-- @
--
-- >>> printX (sampleN 37 (oversampling clk7 clk2 (fromList [(1::Int)..10])))
-- [X,X,1,1,1,2,2,2,2,3,3,3,4,4,4,4,5,5,5,6,6,6,6,7,7,7,8,8,8,8,9,9,9,10,10,10,10]
-- >>> printX (sampleN 12 (almostId clk2 clk7 (fromList [(1::Int)..10])))
-- [X,X,1,2,3,4,5,6,7,8,9,10]
unsafeSynchronizer :: Clock domain1 gated1 -- ^ 'Clock' of the incoming signal
                   -> Clock domain2 gated2 -- ^ 'Clock' of the outgoing signal
                   -> Signal domain1 a
                   -> Signal domain2 a
unsafeSynchronizer (Clock# _ period1 _) (Clock# _ period2 _) s = s'
  where
    t1    = snatToNum period1
    t2    = snatToNum period2
    s' | t1 < t2   = compress   t2 t1 s
       | t1 > t2   = oversample t1 t2 s
       | otherwise = same s

same :: Signal domain1 a -> Signal domain2 a
same (s :- ss) = s :- same ss

oversample :: Int -> Int -> Signal domain1 a -> Signal domain2 a
oversample high low (s :- ss) = s :- oversampleS (reverse (repSchedule high low)) ss

oversampleS :: [Int] -> Signal domain1 a -> Signal domain2 a
oversampleS sched = oversample' sched
  where
    oversample' []     s       = oversampleS sched s
    oversample' (d:ds) (s:-ss) = prefixN d s (oversample' ds ss)

    prefixN 0 _ s = s
    prefixN n x s = x :- prefixN (n-1) x s

compress :: Int -> Int -> Signal domain1 a -> Signal domain2 a
compress high low s = compressS (repSchedule high low) s

compressS :: [Int] -> Signal domain1 a -> Signal domain2 a
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

-- | \"@'delay' clk s@\" delays the values in 'Signal' /s/ for once cycle, the
-- value at time 0 is /undefined/.
--
-- >>> printX (sampleN 3 (delay (systemClock (pure True)) (fromList [1,2,3,4])))
-- [X,1,2]
delay
  :: HasCallStack
  => Clock domain gated
  -- ^ Clock
  -> Signal domain a
  -> Signal domain a
delay = \clk i -> withFrozenCallStack (delay# clk i)
{-# INLINE delay #-}

-- | \"@'register' clk rst i s@\" delays the values in 'Signal' /s/ for one
-- cycle, and sets the value to @i@ the moment the reset becomes 'False'.
--
-- >>> sampleN 3 (register (systemClock (pure True)) systemReset 8 (fromList [1,2,3,4]))
-- [8,1,2]
register
  :: HasCallStack
  => Clock domain gated
  -- ^ clock
  -> Reset domain synchronous
  -- ^ Reset (active-high), 'register' outputs the reset value when the
  -- reset value becomes 'True'
  -> a
  -- ^ Reset value
  -> Signal domain a
  -> Signal domain a
register = \clk rst initial i -> withFrozenCallStack
  (register# clk rst initial i)
{-# INLINE register #-}

-- | Version of 'register' that only updates its content when its fourth
-- argument is a 'Just' value. So given:
--
-- @
-- sometimes1 clk rst = s where
--   s = 'register' clk rst Nothing (switch '<$>' s)
--
--   switch Nothing = Just 1
--   switch _       = Nothing
--
-- countSometimes clk rst = s where
--   s     = 'regMaybe' clk rst 0 (plusM ('pure' '<$>' s) (sometimes1 clk rst))
--   plusM = liftA2 (liftA2 (+))
-- @
--
-- We get:
--
-- >>> sampleN 8 (sometimes1 (systemClock (pure True)) systemReset)
-- [Nothing,Just 1,Nothing,Just 1,Nothing,Just 1,Nothing,Just 1]
-- >>> sampleN 8 (count (systemClock (pure True)) systemReset)
-- [0,0,1,1,2,2,3,3]
regMaybe
  :: HasCallStack
  => Clock domain gated
  -- ^ Clock
  -> Reset domain synchronous
  -- ^ Reset (active-high), 'regMaybe' outputs the reset value when the
  -- reset value becomes 'True'
  -> a
  -- ^ Reset value
  -> Signal domain (Maybe a)
  -> Signal domain a
regMaybe = \clk rst initial iM -> withFrozenCallStack
  (register# (clockGate clk (fmap isJust iM)) rst initial (fmap fromJust iM))
{-# INLINE regMaybe #-}

-- | Version of 'register' that only updates its content when its fourth
-- argument is asserted. So given:
--
-- @
-- oscillate clk rst = let s = 'register' clk rst False (not \<$\> s) in s
-- count clk rst     = let s = 'regEn clk rst 0 (oscillate clk rst) (s + 1) in s
-- @
--
-- We get:
--
-- >>> sampleN 8 (oscillate (systemClock (pure True)) systemReset)
-- [False,True,False,True,False,True,False,True]
-- >>> sampleN 8 (count (systemClock (pure True)) systemReset)
-- [0,0,1,1,2,2,3,3]
regEn
  :: Clock domain clk
  -- ^ Clock
  -> Reset domain synchronous
  -- ^ Reset (active-high), 'regEn' outputs the reset value when the
  -- reset value becomes 'True'
  -> a
  -- ^ Reset value
  -> Signal domain Bool
  -- ^ Enable signal
  -> Signal domain a
  -> Signal domain a
regEn = \clk rst initial en i -> withFrozenCallStack
  (register# (clockGate clk en) rst initial i)
{-# INLINE regEn #-}

-- * Product/Signal isomorphism

-- | Simulate a (@'Unbundled' a -> 'Unbundled' b@) function given a list of
-- samples of type /a/
--
-- >>> simulateB (unbundle . register (systemClock (pure True)) systemReset (8,8) . bundle) [(1,1), (2,2), (3,3)] :: [(Int,Int)]
-- [(8,8),(1,1),(2,2),(3,3)...
-- ...
--
-- __NB__: This function is not synthesisable
simulateB
  :: (Bundle a, Bundle b, NFData a, NFData b)
  => (Unbundled domain1 a -> Unbundled domain2 b)
  -- ^ The function we want to simulate
  -> [a]
  -- ^ Input samples
  -> [b]
simulateB f = simulate (bundle . f . unbundle)

-- | /Lazily/ simulate a (@'Unbundled' a -> 'Unbundled' b@) function given a
-- list of samples of type /a/
--
-- >>> simulateB (unbundle . register (systemClock (pure True)) systemReset (8,8) . bundle) [(1,1), (2,2), (3,3)] :: [(Int,Int)]
-- [(8,8),(1,1),(2,2),(3,3)...
-- ...
--
-- __NB__: This function is not synthesisable
simulateB_lazy
  :: (Bundle a, Bundle b)
  => (Unbundled domain1 a -> Unbundled domain2 b)
  -- ^ The function we want to simulate
  -> [a]
  -- ^ Input samples
  -> [b]
simulateB_lazy f = simulate_lazy (bundle . f . unbundle)
