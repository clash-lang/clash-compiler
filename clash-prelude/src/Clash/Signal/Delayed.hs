{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017     , Google Inc.
                  2019     , Myrtle Software Ltd
                  2021     , LUMI GUIDE FIETSDETECTIE B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Signal.Delayed
  ( -- * Delay-annotated synchronous signals
    DSignal
  , delayed
  , delayedI
  , delayN
  , delayI
  , delayedFold
  , feedback
    -- * Signal \<-\> DSignal conversion
  , fromSignal
  , toSignal
    -- * List \<-\> DSignal conversion (not synthesizable)
  , dfromList
    -- ** lazy versions
  , dfromList_lazy
    -- * Experimental
  , unsafeFromSignal
  , antiDelay
  , forward
  )
where

import           GHC.TypeLits
  (KnownNat, type (^), type (+), type (*))

import Clash.Signal.Delayed.Internal
  (DSignal(..), dfromList, dfromList_lazy, fromSignal, toSignal,
   unsafeFromSignal, antiDelay, feedback, forward)
import qualified Clash.Explicit.Signal.Delayed as E
import           Clash.Sized.Vector
import           Clash.Signal
  (HiddenClock, HiddenClockResetEnable, HiddenEnable, hideClock,
   hideClockResetEnable, hideEnable)

import           Clash.Promoted.Nat            (SNat (..))
import           Clash.XException              (NFDataX)

{- $setup
>>> :set -XDataKinds -XTypeOperators -XTypeApplications -XFlexibleContexts
>>> :m -Clash.Explicit.Prelude
>>> :m -Clash.Explicit.Prelude.Safe
>>> import Clash.Prelude
>>> let delay3 = delayed (-1 :> -1 :> -1 :> Nil)
>>> let delay2 = delayedI :: HiddenClockResetEnable dom  => Int -> DSignal dom n Int -> DSignal dom (n + 2) Int
>>> let delayN2 = delayN d2
>>> let delayI2 = delayI :: (HiddenClock dom, HiddenEnable dom) => Int -> DSignal dom n Int -> DSignal dom (n + 2) Int
>>> let countingSignals = Clash.Prelude.repeat (dfromList [0..]) :: Vec 4 (DSignal dom 0 Int)
-}

-- | Delay a 'DSignal' for @d@ periods.
--
-- @
-- delay3
--   :: HiddenClockResetEnable dom
--   => 'DSignal' dom n Int
--   -> 'DSignal' dom (n + 3) Int
-- delay3 = 'delayed' (-1 ':>' -1 ':>' -1 ':>' 'Nil')
-- @
--
-- >>> sampleN @System 7 (toSignal (delay3 (dfromList [0..])))
-- [-1,-1,-1,-1,1,2,3]
delayed
  :: ( KnownNat d
     , HiddenClockResetEnable dom
     , NFDataX a
     )
  => Vec d a
  -> DSignal dom n a
  -> DSignal dom (n + d) a
delayed = hideClockResetEnable E.delayed

{- | Delay a 'DSignal' for @d@ periods, where @d@ is derived from the context.

@
delay2
  :: HiddenClockResetEnable dom
  => Int
  -> 'DSignal' dom n Int
  -> 'DSignal' dom (n + 2) Int
delay2 = 'delayedI'
@

>>> sampleN @System 7 (toSignal (delay2 (-1) (dfromList [0..])))
[-1,-1,-1,1,2,3,4]

Or @d@ can be specified using type application:

>>> :t delayedI @3
delayedI @3
  :: ... =>
     a -> DSignal dom n a -> DSignal dom (n + 3) a

-}
delayedI
  :: ( KnownNat d
     , NFDataX a
     , HiddenClockResetEnable dom  )
  => a
  -- ^ Initial value
  -> DSignal dom n a
  -> DSignal dom (n + d) a
delayedI = hideClockResetEnable E.delayedI

-- | Delay a 'DSignal' for @d@ cycles, the value at time 0..d-1 is /a/.
--
-- @
-- delayN2
--   :: ( HiddenClock dom
--      , HiddenEnable dom )
--   => Int
--   -> 'DSignal' dom n Int
--   -> 'DSignal' dom (n + 2) Int
-- delayN2 = 'delayN' d2
-- @
--
-- >>> printX $ sampleN @System 6 (toSignal (delayN2 (-1) (dfromList [1..])))
-- [-1,-1,1,2,3,4]
delayN
  :: forall dom  a d n
   . ( HiddenClock dom
     , HiddenEnable dom
     , NFDataX a )
  => SNat d
  -> a
  -- ^ Initial value
  -> DSignal dom n a
  -> DSignal dom (n+d) a
delayN d dflt = hideClock (hideEnable (E.delayN d dflt))

-- | Delay a 'DSignal' for @d@ cycles, where @d@ is derived from the context.
-- The value at time 0..d-1 is a default value.
--
-- @
-- delayI2
--   :: ( HiddenClock dom
--      , HiddenEnable dom )
--   => Int
--   -> 'DSignal' dom n Int
--   -> 'DSignal' dom (n + 2) Int
-- delayI2 = 'delayI'
-- @
--
-- >>> sampleN @System 6 (toSignal (delayI2 (-1) (dfromList [1..])))
-- [-1,-1,1,2,3,4]
--
-- You can also use type application to do the same:
--
-- >>> sampleN @System 6 (toSignal (delayI @2 (-1) (dfromList [1..])))
-- [-1,-1,1,2,3,4]
delayI
  :: forall d n a dom
   . ( HiddenClock dom
     , HiddenEnable dom
     , NFDataX a
     , KnownNat d )
  => a
  -- ^ Initial value
  -> DSignal dom n a
  -> DSignal dom (n+d) a
delayI dflt = hideClock (hideEnable (E.delayI dflt))

-- | Tree fold over a 'Vec' of 'DSignal's with a combinational function,
-- and delaying @delay@ cycles after each application.
-- Values at times 0..(delay*k)-1 are set to a default.
--
-- @
-- countingSignals :: Vec 4 (DSignal dom 0 Int)
-- countingSignals = repeat (dfromList [0..])
-- @
--
-- >>> printX $ sampleN @System 6 (toSignal (delayedFold d1 (-1) (+) countingSignals))
-- [-1,-2,0,4,8,12]
--
-- >>> printX $ sampleN @System 8 (toSignal (delayedFold d2 (-1) (*) countingSignals))
-- [-1,-1,1,1,0,1,16,81]
delayedFold
  :: forall dom  n delay k a
   . ( HiddenClock dom
     , HiddenEnable dom
     , NFDataX a
     , KnownNat delay
     , KnownNat k )
  => SNat delay
  -- ^ Delay applied after each step
  -> a
  -- ^ Initial value
  -> (a -> a -> a)
  -- ^ Fold operation to apply
  -> Vec (2^k) (DSignal dom n a)
  -- ^ Vector input of size 2^k
  -> DSignal dom (n + (delay * k)) a
  -- ^ Output Signal delayed by (delay * k)
delayedFold d dflt f = hideClock (hideEnable (E.delayedFold d dflt f))
