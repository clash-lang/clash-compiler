{-|
Copyright  :  (C) 2013-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
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

module CLaSH.Signal
  ( -- * Implicitly clocked synchronous signal
    Signal, Domain (..), HasReset, HasClock, HasClockReset, hasReset, hasClock
  , withClock, withReset, withClockReset
  , System, SystemClockReset, systemClock, systemReset
  , Clock, Reset, ClockKind (..), ResetKind (..)
  , resetSynchroniser
  , unsafeFromAsyncReset
  , unsafeToAsyncReset
  , fromSyncReset
  , toSyncReset
    -- * Basic circuit functions
  , delay
  , register
  , regMaybe
  , regEn
  , mux
    -- * Testbench functions
  , clockGen
  , asyncResetGen
  , syncResetGen
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

import           CLaSH.Explicit.Signal
  (System, resetSynchroniser, systemClock, systemReset)
import qualified CLaSH.Explicit.Signal as S
import           CLaSH.Promoted.Nat    (SNat (..))
import           CLaSH.Promoted.Symbol (SSymbol (..))
import           CLaSH.Signal.Bundle   (Bundle (..))
import           CLaSH.Signal.Internal hiding
  (sample, sampleN, simulate, simulate_lazy, testFor)
import qualified CLaSH.Signal.Internal as S

{- $setup
>>> :set -XTypeApplications
>>> import CLaSH.XException (printX)
>>> let oscillate = register False (not <$> oscillate)
>>> let count = regEn 0 oscillate (count + 1)
-}

-- * Basic circuit functions

type HasReset domain synchronous = (?rst :: Reset domain synchronous)
type HasClock domain gated       = (?clk :: Clock domain gated)
type HasClockReset domain gated synchronous =
  (HasClock domain gated, HasReset domain synchronous)

hasReset :: HasReset domain synchronous => Reset domain synchronous
hasReset = ?rst
{-# INLINE hasReset #-}

hasClock :: HasClock domain gated => Clock domain gated
hasClock = ?clk
{-# INLINE hasClock #-}

type SystemClockReset = HasClockReset System 'Source 'Asynchronous

withClock
  :: Clock domain gated
  -> (HasClock domain gated => r)
  -> r
withClock clk r
  = let ?clk = clk
    in  r

withReset
  :: Reset domain synchronous
  -> (HasReset domain synchronous => r)
  -> r
withReset rst r
  = let ?rst = rst
    in  r

withClockReset
  :: Clock domain gated
  -> Reset domain synchronous
  -> (HasClockReset domain gated synchronous => r)
  -> r
withClockReset clk rst r
  = let ?clk = clk
        ?rst = rst
    in  r

-- | 'delay' @s@ delays the values in 'Signal' @s@ for once cycle, the value
-- at time 0 is undefined.
--
-- >>> printX (sampleN 3 (delay (fromList [1,2,3,4])))
-- [X,1,2]
delay
  :: (HasClock domain gated, HasCallStack)
  => Signal domain a
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
  -> Signal domain a
  -> Signal domain a
register = \i s -> withFrozenCallStack (register# ?clk ?rst i s)
{-# INLINE register #-}
infixr 3 `register`

regMaybe
  :: (HasClockReset domain gated synchronous, HasCallStack)
  => a
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
  -> Signal domain Bool
  -> Signal domain a
  -> Signal domain a
regEn = \initial en i -> withFrozenCallStack
  (register# (clockGate ?clk en) ?rst initial i)
{-# INLINE regEn #-}

sample
  :: NFData a
  => ((HasClock domain 'Source, HasReset domain 'Asynchronous) =>
      Signal domain a)
  -> [a]
sample s =
  let ?clk = unsafeCoerce (Clock @System (pure True))
      ?rst = unsafeCoerce (Async @System (True :- pure False))
  in  S.sample s

sampleN
  :: NFData a
  => Int
  -> ((HasClock domain 'Source, HasReset domain 'Asynchronous) =>
      Signal domain a)
  -> [a]
sampleN n s =
  let ?clk = unsafeCoerce (Clock @System (pure True))
      ?rst = unsafeCoerce (Async @System (True :- pure False))
  in  S.sampleN n s

simulate
  :: (NFData a, NFData b)
  => ((HasClock domain 'Source, HasReset domain 'Asynchronous) =>
      Signal domain a -> Signal domain b)
  -> [a]
  -> [b]
simulate f =
  let ?clk = unsafeCoerce (Clock @System (pure True))
      ?rst = unsafeCoerce (Async @System (True :- pure False))
  in  S.simulate f

simulate_lazy
  :: ((HasClock domain 'Source, HasReset domain 'Asynchronous) =>
      Signal domain a -> Signal domain b)
  -> [a]
  -> [b]
simulate_lazy f =
  let ?clk = unsafeCoerce (Clock @System (pure True))
      ?rst = unsafeCoerce (Async @System (True :- pure False))
  in  S.simulate_lazy f

simulateB
  :: (Bundle a, Bundle b, NFData a, NFData b)
  => ((HasClock domain 'Source, HasReset domain 'Asynchronous) =>
      Unbundled domain a -> Unbundled domain b)
  -> [a]
  -> [b]
simulateB f =
  let ?clk = unsafeCoerce (Clock @System (pure True))
      ?rst = unsafeCoerce (Async @System (True :- pure False))
  in  S.simulateB f

simulateB_lazy
  :: (Bundle a, Bundle b)
  => ((HasClock domain 'Source, HasReset domain 'Asynchronous) =>
      Unbundled domain a -> Unbundled domain b)
  -> [a]
  -> [b]
simulateB_lazy f =
  let ?clk = unsafeCoerce (Clock @System (pure True))
      ?rst = unsafeCoerce (Async @System (True :- pure False))
  in  S.simulateB_lazy f

testFor
  :: Int
  -> ((HasClock domain 'Source, HasReset domain 'Asynchronous) =>
      Signal domain Bool)
  -> Property
testFor n s = property (and (CLaSH.Signal.sampleN n s))
