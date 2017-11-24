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
-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Signal
  ( -- * Synchronous signals
    Signal
  , Domain (..)
  , System
    -- * Clock
  , Clock
  , ClockKind (..)
    -- * Reset
  , Reset
  , ResetKind (..)
  , unsafeFromAsyncReset
  , unsafeToAsyncReset
  , fromSyncReset
  , unsafeToSyncReset
  , resetSynchronizer
    -- * Implicit routing of clocks and resets
    -- $implicitclockandreset

    -- ** Implicit clock
  , HasClock
  , hasClock
  , withClock
    -- ** Implicit reset
  , HasReset
  , hasReset
  , withReset
    -- ** Implicit clock and reset
  , HasClockReset
  , withClockReset
  , SystemClockReset
    -- * Basic circuit functions
  , delay
  , register
  , regMaybe
  , regEn
  , mux
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

import           Control.DeepSeq       (NFData)
import           GHC.Stack             (HasCallStack, withFrozenCallStack)
import           GHC.TypeLits          (KnownNat, KnownSymbol)
import           Data.Bits             (Bits) -- Haddock only
import           Data.Maybe            (isJust, fromJust)
import           Test.QuickCheck       (Property, property)
import           Unsafe.Coerce         (unsafeCoerce)

import           Clash.Explicit.Signal
  (System, resetSynchronizer, systemClockGen, systemResetGen, tbSystemClockGen)
import qualified Clash.Explicit.Signal as S
import           Clash.Promoted.Nat    (SNat (..))
import           Clash.Promoted.Symbol (SSymbol (..))
import           Clash.Signal.Bundle   (Bundle (..))
import           Clash.Signal.Internal hiding
  (sample, sample_lazy, sampleN, sampleN_lazy, simulate, simulate_lazy, testFor)
import qualified Clash.Signal.Internal as S

{- $setup
>>> :set -XTypeApplications
>>> import Clash.XException (printX)
>>> import Control.Applicative (liftA2)
>>> let oscillate = register False (not <$> oscillate)
>>> let count = regEn 0 oscillate (count + 1)
>>> :{
sometimes1 = s where
  s = register Nothing (switch <$> s)
  switch Nothing = Just 1
  switch _       = Nothing
:}

>>> :{
countSometimes = s where
  s     = regMaybe 0 (plusM (pure <$> s) sometimes1)
  plusM = liftA2 (liftA2 (+))
:}

-}

-- * Implicit routing of clock and reset signals

{- $implicitclockandreset #implicitclockandreset#
Clocks and resets are by default implicitly routed to their components. You can
see from the type of a component whether it has implicitly routed clocks or
resets:

It has an implicitly routed clock when it has a:

@
f :: 'HasClock' domain gated => ...
@

Constraint.

Or it has an implicitly routed reset when it has a:

@
g :: 'HasReset' domain synchronous => ...
@

Constraint.

Or it has both an implicitly routed clock and an implicitly routed reset when it
has a:

@
h :: 'HasClockReset' domain gated synchronous => ..
@

Constraint.

Given a component with an explicit clock and reset port, you can have them
implicitly routed using 'hasClock' and 'hasReset'. So given a:

@
f :: Clock domain gated -> Reset domain synchronous -> Signal domain a -> ...
@

You have have the clock and reset implicitly routed by:

@
-- g :: 'HasClockReset' domain gated synchronous => Signal domain a -> ...
g = f 'hasClock' 'hasReset'
@

=== Assigning explicit clock and reset arguments to implicit clocks and resets

Given a component:

@
f :: HasClockReset domain gated synchronous
  => Signal domain Int
  -> Signal domain Int
@

which has an implicitly routed clock and implicitly routed clock, we can use
it in a function with explicit clock and reset arguments like so:

@
-- g :: Clock domain gated -> Reset domain synchronous -> Signal domain Int -> Signal domain Int
g clk rst = 'withClockReset' clk rst f
@

Similarly, there are 'withClock' and 'withReset' to connect just implicit clocks
or just implicit resets respectively.

You will need to explicitly apply clocks and resets when you want to using
components such as PPLs and 'resetSynchronizer':

@
topEntity
  :: Clock System Source
  -> Reset System Asynchronous
  -> Signal System Int
  -> Signal System Int
topEntity clk rst =
  let (pllOut,pllStable) = 'Clash.Intel.ClockGen.altpll' (SSymbol \@\"altpll50\") clk rst
      rstSync            = 'resetSynchronizer' pllOut ('unsafeToAsyncReset' pllStable)
  in  'withClockReset' pllOut rstSync f
@

=== Implicit parameters

__TL;DR__ do __not__ use 'HasClock', 'HasReset', 'HasClockReset', 'hasClock',
'hasReset', or 'SystemClockReset', when you have multiple /clock/ (or /reset/)
domains.

__Want to know:__

Under the hood, Clash uses
<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#implicit-parameters Implicit Parameters>
to implicitly route the clock and reset.

This means that you cannot use the 'hasClock' and 'hasReset' functions when you
are working with multiple /clock/ (and /reset/) domains. Because, given a:

@
f :: Clock domainA gatedA -> Clock domainB gatedB -> Signal domainA a -> Signal domainB b -> ..
@

and using 'hasClock' as we did above, we will get:

@
-- g :: HasClock domainB gatedB => Signal domainB a -> Signal domainB b -> ...
g = f hasClock hasClock
@

That is, sub-components of /f/ will be synchronized to the same clock, where
the idea was probably to synchronize them to different clocks.

Trying to give different implicit clocks will also not work:

@
h :: forall domainA domainB gatedA gatedB a b
   . (HasClock domainA gatedA, HasClock domainB gatedB)
  => Signal domainA a -> Signal domainB b -> ...
h = f (hasClock \@domainA) (hasClock \@domainB)
@

as it will result in a type error.

In case you want to have implicitly routed clocks and resets for
multi-/clock/ (and -/reset/) domain designs you will have to use to use
<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#implicit-parameters Implicit Parameters>
directly:

@
-- k :: (?clkB::Clock domainB gatedB,?clkA::Clock domainA gatedA)
--   => Signal domainA a -> Signal domainB b -> ...
k = f ?clkA ?clkB
@

Note that you __cannot__ use /any/ of the functions that mention 'HasClock',
'HasReset', or 'HasClockReset' constraints. So when you want to assign clock
or reset arguments to your self-rolled implicit clocks and resets you will need
to use the

@
let ?clkA = ...
in  k
@

notation for binding
<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#implicit-parameters Implicit Parameters>.
-}

-- | A /constraint/ that indicates the component needs a 'Clock'
--
-- <Clash-Signal.html#implicitclockandreset Click here to read more about implicit clocks and resets>
type HasClock domain gated       = (?clk :: Clock domain gated)

-- | A /constraint/ that indicates the component needs a 'Reset'
--
-- <Clash-Signal.html#implicitclockandreset Click here to read more about implicit clocks and resets>
type HasReset domain synchronous = (?rst :: Reset domain synchronous)

-- | A /constraint/ that indicates the component needs a 'Clock' and 'Reset'
--
-- <Clash-Signal.html#implicitclockandreset Click here to read more about implicit clocks and resets>
type HasClockReset domain gated synchronous =
  (HasClock domain gated, HasReset domain synchronous)

-- | For a component with an explicit clock port, implicitly route a clock
-- to that port.
--
-- So given:
--
-- > f :: Clock domain gated -> Signal domain a -> ...
--
-- You can implicitly route a clock by:
--
-- > g :: HaClock domain gated => Signal domain a -> ...
-- > g = f hasClock
--
-- __NB__ all components with a `HasClock` /constraint/ are connected to
-- the same clock.
--
-- <Clash-Signal.html#implicitclockandreset Click here to read more about implicit clocks and resets>
hasClock :: HasClock domain gated => Clock domain gated
hasClock = ?clk
{-# INLINE hasClock #-}

-- | For a component with an explicit clock port, implicitly route a clock
-- to that port.
--
-- So given:
--
-- > f :: Reset domain synchronous -> Signal domain a -> ...
--
-- You can implicitly route a clock by:
--
-- > g :: HasReset domain synchronous => Signal domain a -> ...
-- > g = f hasReset
--
-- __NB__ all components with a `HasReset` /constraint/ are connected to
-- the same reset.
--
-- <Clash-Signal.html#implicitclockandreset Click here to read more about implicit clocks and resets>
hasReset :: HasReset domain synchronous => Reset domain synchronous
hasReset = ?rst
{-# INLINE hasReset #-}

-- | A /constraint/ that indicates the component needs a normal 'Clock' and
-- an asynchronous 'Reset' belonging to the 'System' domain.
--
-- <Clash-Signal.html#implicitclockandreset Click here to read more about implicit clocks and resets>
type SystemClockReset = HasClockReset System 'Source 'Asynchronous

-- | Explicitly connect a 'Clock' to a component whose clock is implicitly
-- routed
--
-- <Clash-Signal.html#implicitclockandreset Click here to read more about implicit clocks and resets>
withClock
  :: Clock domain gated
  -- ^ The 'Clock' we want to connect
  -> (HasClock domain gated => r)
  -- ^ The component with an implicitly routed clock
  -> r
withClock clk r
  = let ?clk = clk
    in  r

-- | Explicit connect a 'Reset' to a component whose reset is implicitly
-- routed
--
-- <Clash-Signal.html#implicitclockandreset Click here to read more about implicit clocks and resets>
withReset
  :: Reset domain synchronous
  -- ^ The 'Reset' we want to connect
  -> (HasReset domain synchronous => r)
  -- ^ The component with an implicitly routed reset
  -> r
withReset rst r
  = let ?rst = rst
    in  r

-- | Explicitly connect a 'Clock' and 'Reset' to a component whose clock and
-- reset are implicitly routed
--
-- <Clash-Signal.html#implicitclockandreset Click here to read more about implicit clocks and resets>
--
-- === __Example__
--
-- @
-- topEntity :: Vec 2 (Vec 3 (Unsigned 8)) -> Vec 6 (Unsigned 8)
-- topEntity = concat
--
-- testBench :: Signal System Bool
-- testBench = done'
--   where
--     testInput      = pure ((1 :> 2 :> 3 :> Nil) :> (4 :> 5 :> 6 :> Nil) :> Nil)
--     expectedOutput = outputVerifier ((1:>2:>3:>4:>5:>6:>Nil):>Nil)
--     done           = expectedOutput (topEntity <$> testInput)
--     done'          = 'withClockReset' (tbSystemClockGen (not <\$\> done')) systemResetGen done
-- @
withClockReset
  :: Clock domain gated
  -- ^ The 'Clock' we want to connect
  -> Reset domain synchronous
  -- ^ The 'Reset' we want to connect
  -> (HasClockReset domain gated synchronous => r)
  -- ^ The component with an implicitly routed clock and reset
  -> r
withClockReset clk rst r
  = let ?clk = clk
        ?rst = rst
    in  r

-- * Basic circuit functions

-- | 'delay' @s@ delays the values in 'Signal' @s@ for once cycle, the value
-- at time 0 is undefined.
--
-- >>> printX (sampleN 3 (delay (fromList [1,2,3,4])))
-- [X,1,2]
delay
  :: (HasClock domain gated, HasCallStack)
  => Signal domain a
  -- ^ Signal to delay
  -> Signal domain a
delay = \i -> withFrozenCallStack (delay# ?clk i)
{-# INLINE delay #-}

-- | 'register' @i s@ delays the values in 'Signal' @s@ for one cycle, and sets
-- the value at time 0 to @i@
--
-- >>> sampleN 3 (register 8 (fromList [1,2,3,4]))
-- [8,1,2]
register
  :: (HasClockReset domain gated synchronous, HasCallStack)
  => a
  -- ^ Reset value
  --
  -- 'register' has an /active-hig/h 'Reset', meaning that 'register' outputs the
  -- reset value when the reset value becomes 'True'
  -> Signal domain a
  -> Signal domain a
register = \i s -> withFrozenCallStack (register# ?clk ?rst i s)
{-# INLINE register #-}
infixr 3 `register`

-- | Version of 'register' that only updates its content when its second
-- argument is a 'Just' value. So given:
--
-- @
-- sometimes1 = s where
--   s = 'register' Nothing (switch '<$>' s)
--
--   switch Nothing = Just 1
--   switch _       = Nothing
--
-- countSometimes = s where
--   s     = 'regMaybe' 0 (plusM ('pure' '<$>' s) sometimes1)
--   plusM = 'liftA2' (liftA2 (+))
-- @
--
-- We get:
--
-- >>> sampleN 8 sometimes1
-- [Nothing,Just 1,Nothing,Just 1,Nothing,Just 1,Nothing,Just 1]
-- >>> sampleN 8 countSometimes
-- [0,0,1,1,2,2,3,3]
regMaybe
  :: (HasClockReset domain gated synchronous, HasCallStack)
  => a
  -- ^ Reset value
  --
  -- 'regMaybe' has an /active-high/ 'Reset', meaning that 'regMaybe' outputs the
  -- reset value when the reset value becomes 'True'
  -> Signal domain (Maybe a)
  -> Signal domain a
regMaybe = \initial iM -> withFrozenCallStack
  (register# (clockGate ?clk (fmap isJust iM)) ?rst initial (fmap fromJust iM))
{-# INLINE regMaybe #-}
infixr 3 `regMaybe`

-- | Version of 'register' that only updates its content when its second argument
-- is asserted. So given:
--
-- @
-- oscillate = 'register' False ('not' '<$>' oscillate)
-- count     = 'regEn' 0 oscillate (count + 1)
-- @
--
-- We get:
--
-- >>> sampleN 8 oscillate
-- [False,True,False,True,False,True,False,True]
-- >>> sampleN 8 count
-- [0,0,1,1,2,2,3,3]
regEn
  :: (HasClockReset domain gated synchronous, HasCallStack)
  => a
  -- ^ Reset value
  --
  -- 'regEn' has an /active-high/ 'Reset', meaning that 'regEn' outputs the
  -- reset value when the reset value becomes 'True'
  -> Signal domain Bool
  -> Signal domain a
  -> Signal domain a
regEn = \initial en i -> withFrozenCallStack
  (register# (clockGate ?clk en) ?rst initial i)
{-# INLINE regEn #-}

-- * Signal -> List conversion

-- | Get an infinite list of samples from a 'Clash.Signal.Signal'
--
-- The elements in the list correspond to the values of the 'Signal'
-- at consecutive clock cycles
--
-- > sample s == [s0, s1, s2, s3, ...
--
-- __NB__: This function is not synthesisable
sample
  :: NFData a
  => ((HasClockReset domain 'Source 'Asynchronous) => Signal domain a)
  -- ^ 'Signal' we want to sample, whose source potentially needs an implicitly
  -- routed clock (and reset)
  -> [a]
sample s =
  let ?clk = unsafeCoerce (clockGen @System)
      ?rst = unsafeCoerce (Async @System (True :- pure False))
  in  S.sample s

-- | Get a list of /n/ samples from a 'Signal'
--
-- The elements in the list correspond to the values of the 'Signal'
-- at consecutive clock cycles
--
-- > sampleN 3 s == [s0, s1, s2]
--
-- __NB__: This function is not synthesisable
sampleN
  :: NFData a
  => Int
  -- ^ The number of samples we want to see
  -> ((HasClockReset domain 'Source 'Asynchronous) => Signal domain a)
  -- ^ 'Signal' we want to sample, whose source potentially needs an implicitly
  -- routed clock (and reset)
  -> [a]
sampleN n s =
  let ?clk = unsafeCoerce (Clock @System SSymbol SNat)
      ?rst = unsafeCoerce (Async @System (True :- pure False))
  in  S.sampleN n s

-- | /Lazily/ get an infinite list of samples from a 'Clash.Signal.Signal'
--
-- The elements in the list correspond to the values of the 'Signal'
-- at consecutive clock cycles
--
-- > sample s == [s0, s1, s2, s3, ...
--
-- __NB__: This function is not synthesisable
sample_lazy
  :: ((HasClockReset domain 'Source 'Asynchronous) => Signal domain a)
  -- ^ 'Signal' we want to sample, whose source potentially needs an implicitly
  -- routed clock (and reset)
  -> [a]
sample_lazy s =
  let ?clk = unsafeCoerce (Clock @System SSymbol SNat)
      ?rst = unsafeCoerce (Async @System (True :- pure False))
  in  S.sample_lazy s


-- | Lazily get a list of /n/ samples from a 'Signal'
--
-- The elements in the list correspond to the values of the 'Signal'
-- at consecutive clock cycles
--
-- > sampleN 3 s == [s0, s1, s2]
--
-- __NB__: This function is not synthesisable
sampleN_lazy
  :: Int
  -> ((HasClockReset domain 'Source 'Asynchronous) => Signal domain a)
  -- ^ 'Signal' we want to sample, whose source potentially needs an implicitly
  -- routed clock (and reset)
  -> [a]
sampleN_lazy n s =
  let ?clk = unsafeCoerce (Clock @System SSymbol SNat)
      ?rst = unsafeCoerce (Async @System (True :- pure False))
  in  S.sampleN_lazy n s

-- * Simulation functions

-- | Simulate a (@'Signal' a -> 'Signal' b@) function given a list of samples
-- of type /a/
--
-- >>> simulate (register 8) [1, 2, 3]
-- [8,1,2,3...
-- ...
--
-- __NB__: This function is not synthesisable
simulate
  :: (NFData a, NFData b)
  => ((HasClockReset domain 'Source 'Asynchronous) =>
      Signal domain a -> Signal domain b)
  -- ^ Function we want to simulate, whose components potentially needs an
  -- implicitly routed clock (and reset)
  -> [a]
  -> [b]
simulate f =
  let ?clk = unsafeCoerce (Clock @System SSymbol SNat)
      ?rst = unsafeCoerce (Async @System (True :- pure False))
  in  S.simulate f

-- | /Lazily/ simulate a (@'Signal' a -> 'Signal' b@) function given a list of
-- samples of type /a/
--
-- >>> simulate (register 8) [1, 2, 3]
-- [8,1,2,3...
-- ...
--
-- __NB__: This function is not synthesisable
simulate_lazy
  :: ((HasClockReset domain 'Source 'Asynchronous) =>
      Signal domain a -> Signal domain b)
  -- ^ Function we want to simulate, whose components potentially needs an
  -- implicitly routed clock (and reset)
  -> [a]
  -> [b]
simulate_lazy f =
  let ?clk = unsafeCoerce (Clock @System SSymbol SNat)
      ?rst = unsafeCoerce (Async @System (True :- pure False))
  in  S.simulate_lazy f

-- | Simulate a (@'Unbundled' a -> 'Unbundled' b@) function given a list of
-- samples of type @a@
--
-- >>> simulateB (unbundle . register (8,8) . bundle) [(1,1), (2,2), (3,3)] :: [(Int,Int)]
-- [(8,8),(1,1),(2,2),(3,3)...
-- ...
--
-- __NB__: This function is not synthesisable
simulateB
  :: (Bundle a, Bundle b, NFData a, NFData b)
  => ((HasClockReset domain 'Source 'Asynchronous) =>
      Unbundled domain a -> Unbundled domain b)
  -- ^ Function we want to simulate, whose components potentially needs an
  -- implicitly routed clock (and reset)
  -> [a]
  -> [b]
simulateB f =
  let ?clk = unsafeCoerce (Clock @System SSymbol SNat)
      ?rst = unsafeCoerce (Async @System (True :- pure False))
  in  S.simulateB f

-- | /Lazily/ simulate a (@'Unbundled' a -> 'Unbundled' b@) function given a
-- list of samples of type @a@
--
-- >>> simulateB (unbundle . register (8,8) . bundle) [(1,1), (2,2), (3,3)] :: [(Int,Int)]
-- [(8,8),(1,1),(2,2),(3,3)...
-- ...
--
-- __NB__: This function is not synthesisable
simulateB_lazy
  :: (Bundle a, Bundle b)
  => ((HasClockReset domain 'Source 'Asynchronous) =>
      Unbundled domain a -> Unbundled domain b)
  -- ^ Function we want to simulate, whose components potentially needs an
  -- implicitly routed clock (and reset)
  -> [a]
  -> [b]
simulateB_lazy f =
  let ?clk = unsafeCoerce (Clock @System SSymbol SNat)
      ?rst = unsafeCoerce (Async @System (True :- pure False))
  in  S.simulateB_lazy f

-- * QuickCheck combinators

-- |  @testFor n s@ tests the signal /s/ for /n/ cycles.
testFor
  :: Int
  -- ^ The number of cycles we want to test for
  -> ((HasClockReset domain 'Source 'Asynchronous) => Signal domain Bool)
  -- ^ 'Signal' we want to evaluate, whose source potentially needs an
  -- implicitly routed clock (and reset)
  -> Property
testFor n s = property (and (Clash.Signal.sampleN n s))
