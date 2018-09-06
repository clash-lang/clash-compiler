{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2016     , Myrtle Software,
                  2017     , Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

CλaSH has synchronous 'Signal's in the form of:

@
'Signal' (domain :: 'Domain') a
@

Where /a/ is the type of the value of the 'Signal', for example /Int/ or /Bool/,
and /domain/ is the /clock-/ (and /reset-/) domain to which the memory elements
manipulating these 'Signal's belong.

The type-parameter, /domain/, is of the kind 'Domain' which has types of the
following shape:

@
data Domain = Dom { domainName :: 'GHC.TypeLits.Symbol', clkPeriod :: 'GHC.TypeLits.Nat' }
@

Where /domainName/ is a type-level string ('GHC.TypeLits.Symbol') representing
the name of the /clock-/ (and /reset-/) domain, and /clkPeriod/ is a type-level
natural number ('GHC.TypeLits.Nat') representing the clock period (in __ps__)
of the clock lines in the /clock-domain/.

* __NB__: \"Bad things\"™  happen when you actually use a clock period of @0@,
so do __not__ do that!
* __NB__: You should be judicious using a clock with period of @1@ as you can
never create a clock that goes any faster!

=== Explicit clocks and resets, and meta-stability #metastability#

When <Clash-Signal.html#hiddenclockandreset clocks and resets are implicitly routed>
using the mechanisms provided by the __clash-prelude__, then clocks and resets
are also implicitly unique.

The protection against accidental
<https://en.wikipedia.org/wiki/Metastability_in_electronics metastability>
offered by Clash's /domain/ annotation on 'Signal's is based on the uniqueness
of clocks and resets. But with explicit clock and reset lines, there are
ways to (accidentally) introduce situations that are prone to metastability.

There are four different clock and reset lines:

@
'Reset' domain 'Synchronous'
'Reset' domain 'Asynchronous'
'Clock' domain 'Source'
'Clock' domain 'Gated'
@

We now go over the combinations over these clock and reset line combinations
and explain when they can potentially introduce situations prone to
meta-stability:

    *   /Reset situation 1/:

        @
        f :: Reset domain Synchronous -> Reset domain Synchronous -> ..
        f x y = ..
        @

        There are no problems here, because although /x/ and /y/ can have
        different values, components to these reset lines are reset
        /synchronously/, and there is no metastability situation.

    *   /Reset situation 2/:

        @
        g :: Reset domain Asynchronous -> Reset domain Asynchronous -> ..
        g x y = ..
        @

        This situation can be prone to metastability, because although /x/ and
        /y/ belong to the same /domain/ according to their type, there is no
        guarantee that they actually originate from the same source. This means
        that one component can enter its reset state asynchronously to another
        component, inducing metastability in the other component.

        * The Clash compiler will give a warning whenever a function has a
          type-signature similar to the one above.
        * This is the reason why `unsafeFromAsyncReset` is prefixed with the
          word /unsafe/.

    *   /Reset situation 3/:

        @
        h :: Reset domain Asynchronous -> Reset domain Synchronous -> ..
        h x y = ..
        @

        Also this situation is prone to metastability, because again, one
        component can enter its reset state asynchronously to the other,
        inducing metastability in the other component.

          * The Clash compiler will give a warning whenever a function has a
          type-signature similar to the one above.
          * Although in a standalone context, converting between @'Reset' domain
          'Synchronous'@ and @'Signal' domain 'Bool'@ would be safe from a
          metastability point of view, it is not when we're in a context where
          there are also asynchronous resets. That is why 'unsafeToSyncReset'
          is prefixed with the word /unsafe/.

    *   /Clock situations 1, 2, and 3/:

        @
        k :: Clock domain Source -> Clock domain Source -> ..
        k x y = ..

        l :: Clock domain Source -> Clock domain Gated -> ..
        l x y = ..

        m :: Clock domain Gated -> Clock domain Gated -> ..
        m x y = ..
        @

        All the above situations are potentially prone to metastability, because
        even though /x/ and /y/ belong to the same /domain/ according to their
        type, there is no guarantee that they actually originate from the same
        source. They could hence be connected to completely unrelated clock
        sources, and components can then induce metastable states in others.

        * The Clash compiler will give a warning whenever a function has a
        type-signature similar to one of the above three situations.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE MagicHash #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Explicit.Signal
  ( -- * Synchronous signal
    Signal, Domain (..), System
    -- * Clock
  , Clock, ClockKind (..)
  , freqCalc
    -- ** Synchronisation primitive
  , unsafeSynchronizer
    -- ** Clock gating
  , clockGate
    -- * Reset
  , Reset, ResetKind (..)
  , unsafeFromAsyncReset
  , unsafeToAsyncReset
  , fromSyncReset
  , unsafeToSyncReset
  , resetSynchronizer
    -- * Basic circuit functions
  , delay
  , register
  , regMaybe
  , regEn
    -- * Simulation and testbench functions
  , clockGen
  , tbClockGen
  , asyncResetGen
  , syncResetGen
  , systemClockGen
  , tbSystemClockGen
  , systemResetGen
    -- * Boolean connectives
  , (.&&.), (.||.)
    -- * Product/Signal isomorphism
  , Bundle(..)
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
import Data.Maybe            (isJust, fromJust)
import GHC.Stack             (HasCallStack, withFrozenCallStack)

import Clash.Signal.Internal
import Clash.Signal.Bundle   (Bundle (..))

import Clash.XException      (Undefined)

{- $setup
>>> :set -XDataKinds -XTypeApplications
>>> import Clash.Explicit.Prelude
>>> import qualified Data.List as L
>>> type Dom2 = Dom "dom" 2
>>> type Dom7 = Dom "dom" 7
>>> let clk2 = clockGen @Dom2
>>> let clk7 = clockGen @Dom7
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

-- **Clock

-- | A /clock/ (and /reset/) domain with clocks running at 100 MHz
type System = 'Dom "system" 10000

-- | Clock generator for the 'System' clock domain.
--
-- __NB__: should only be used for simulation, and __not__ for the /testBench/
-- function. For the /testBench/ function, used 'tbSystemClockGen'
systemClockGen
  :: Clock System 'Source
systemClockGen = clockGen

-- | Clock generator for the 'System' clock domain.
--
-- __NB__: can be used in the /testBench/ function
--
-- === __Example__
--
-- @
-- topEntity :: Vec 2 (Vec 3 (Unsigned 8)) -> Vec 6 (Unsigned 8)
-- topEntity = concat
--
-- testBench :: Signal System Bool
-- testBench = done
--   where
--     testInput      = pure ((1 :> 2 :> 3 :> Nil) :> (4 :> 5 :> 6 :> Nil) :> Nil)
--     expectedOutput = outputVerifier ((1:>2:>3:>4:>5:>6:>Nil):>Nil)
--     done           = exposeClockReset (expectedOutput (topEntity <$> testInput)) clk rst
--     clk            = 'tbSystemClockGen' (not <\$\> done)
--     rst            = systemResetGen
-- @
tbSystemClockGen
  :: Signal System Bool
  -> Clock System 'Source
tbSystemClockGen = tbClockGen

-- | Reset generator for the 'System' clock domain.
--
-- __NB__: should only be used for simulation or the \testBench\ function.
--
-- === __Example__
--
-- @
-- topEntity :: Vec 2 (Vec 3 (Unsigned 8)) -> Vec 6 (Unsigned 8)
-- topEntity = concat
--
-- testBench :: Signal System Bool
-- testBench = done
--   where
--     testInput      = pure ((1 :> 2 :> 3 :> Nil) :> (4 :> 5 :> 6 :> Nil) :> Nil)
--     expectedOutput = outputVerifier ((1:>2:>3:>4:>5:>6:>Nil):>Nil)
--     done           = exposeClockReset (expectedOutput (topEntity <$> testInput)) clk rst
--     clk            = tbSystemClockGen (not <\$\> done)
--     rst            = 'systemResetGen'
-- @
systemResetGen :: Reset System 'Asynchronous
systemResetGen = asyncResetGen

-- | Normally, asynchronous resets can be both asynchronously asserted and
-- de-asserted. Asynchronous de-assertion can induce meta-stability in the
-- component which is being reset. To ensure this doesn't happen,
-- 'resetSynchronizer' ensures that de-assertion of a reset happens
-- synchronously. Assertion of the reset remains asynchronous.
--
-- Note that asynchronous assertion does not induce meta-stability in the
-- component whose reset is asserted. However, when a component \"A\" in another
-- clock or reset domain depends on the value of a component \"B\" being
-- reset, then asynchronous assertion of the reset of component \"B"\ can induce
-- meta-stability in component \"A\". To prevent this from happening you need
-- to use a proper synchronizer, for example one of the synchronizers in
-- "Clash.Explicit.Synchronizer"
--
-- __NB:__ Assumes the component(s) being reset have an /active-high/ reset port,
-- which all components in __clash-prelude__ have.
--
-- === __Example__
--
-- @
-- topEntity
--   :: Clock  System Source
--   -> Reset  System Asynchronous
--   -> Signal System Bit
--   -> Signal System (BitVector 8)
-- topEntity clk rst key1 =
--     let  (pllOut,pllStable) = altpll (SSymbol @ "altpll50") clk rst
--          rstSync            = 'resetSynchronizer' pllOut (unsafeToAsyncReset pllStable)
--     in   exposeClockReset leds pllOut rstSync
--   where
--     key1R  = isRising 1 key1
--     leds   = mealy blinkerT (1,False,0) key1R
-- @
resetSynchronizer
  :: Clock domain gated
  -> Reset domain 'Asynchronous
  -> Reset domain 'Asynchronous
resetSynchronizer clk rst  =
  let r1 = register clk rst True (pure False)
      r2 = register clk rst True r1
  in  unsafeToAsyncReset r2

-- | Calculate the period, in __ps__, given a frequency in __Hz__
--
-- i.e. to calculate the clock period for a circuit to run at 240 MHz we get
--
-- >>> freqCalc 240e6
-- 4167
--
-- __NB__: This function is /not/ synthesisable
freqCalc :: Double -> Integer
freqCalc freq = ceiling ((1.0 / freq) / 1.0e-12)

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
-- clk7 = 'clockGen'
-- @
--
-- and
--
-- @
-- type Dom2 = 'Dom' \"dom\" 2
--
-- clk2 :: 'Clock' Dom2 Source
-- clk2 = 'clockGen'
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
unsafeSynchronizer
  :: Clock  domain1 gated1 -- ^ 'Clock' of the incoming signal
  -> Clock  domain2 gated2 -- ^ 'Clock' of the outgoing signal
  -> Signal domain1 a
  -> Signal domain2 a
unsafeSynchronizer clk1 clk2 s = s'
  where
    t1    = clockPeriod clk1
    t2    = clockPeriod clk2
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
-- >>> printX (sampleN 3 (delay systemClockGen (fromList [1,2,3,4])))
-- [X,1,2]
delay
  :: (HasCallStack, Undefined a)
  => Clock domain gated
  -- ^ Clock
  -> Signal domain a
  -> Signal domain a
delay = \clk i -> withFrozenCallStack (delay# clk i)
{-# INLINE delay #-}

-- | \"@'register' clk rst i s@\" delays the values in 'Signal' /s/ for one
-- cycle, and sets the value to @i@ the moment the reset becomes 'False'.
--
-- >>> sampleN 3 (register systemClockGen systemResetGen 8 (fromList [1,2,3,4]))
-- [8,1,2]
register
  :: (HasCallStack, Undefined a)
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
-- >>> sampleN 8 (sometimes1 systemClockGen systemResetGen)
-- [Nothing,Just 1,Nothing,Just 1,Nothing,Just 1,Nothing,Just 1]
-- >>> sampleN 8 (count systemClockGen systemResetGen)
-- [0,0,1,1,2,2,3,3]
regMaybe
  :: (HasCallStack, Undefined a)
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
-- >>> sampleN 8 (oscillate systemClockGen systemResetGen)
-- [False,True,False,True,False,True,False,True]
-- >>> sampleN 8 (count systemClockGen systemResetGen)
-- [0,0,1,1,2,2,3,3]
regEn
  :: Undefined a
  => Clock domain clk
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
-- >>> simulateB (unbundle . register systemClockGen systemResetGen (8,8) . bundle) [(1,1), (2,2), (3,3)] :: [(Int,Int)]
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
-- >>> simulateB (unbundle . register systemClockGen systemResetGen (8,8) . bundle) [(1,1), (2,2), (3,3)] :: [(Int,Int)]
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
