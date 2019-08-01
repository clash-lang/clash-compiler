{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017     , Google Inc.
                  2019     , Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType #-}
#endif
#if __GLASGOW_HASKELL__ < 806
{-# LANGUAGE TypeInType #-}
#endif

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

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
  )
where

import           Data.Coerce                   (coerce)
import           Data.Kind                     (Type)
import           Data.Proxy                    (Proxy(..))
import           GHC.TypeLits
  (KnownNat, type (^), type (+), type (*), Nat)
import           Data.Singletons.Prelude       (Apply, TyFun, type (@@))

import           Clash.Signal.Internal         (Domain)
import Clash.Signal.Delayed.Internal
  (DSignal(..), dfromList, dfromList_lazy, fromSignal, toSignal,
   unsafeFromSignal, antiDelay, feedback)
import qualified Clash.Explicit.Signal.Delayed as E
import           Clash.Sized.Vector            (Vec, dtfold)
import           Clash.Signal
  (HiddenClockResetEnable , hideClockResetEnable, Signal, delay)

import           Clash.Promoted.Nat            (SNat (..), snatToInteger)
import           Clash.XException              (Undefined)

{- $setup
>>> :set -XDataKinds -XTypeOperators -XTypeApplications -XFlexibleContexts
>>> import Clash.Prelude
>>> let delay3 = delayed (-1 :> -1 :> -1 :> Nil)
>>> let delay2 = delayedI :: HiddenClockResetEnable dom  => Int -> DSignal dom n Int -> DSignal dom (n + 2) Int
>>> let delayN2 = delayN d2
>>> let delayI2 = delayI :: HiddenClockResetEnable dom  => Int -> DSignal dom n Int -> DSignal dom (n + 2) Int
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
     , Undefined a
     )
  => Vec d a
  -> DSignal dom n a
  -> DSignal dom (n + d) a
delayed = hideClockResetEnable E.delayed

-- | Delay a 'DSignal' for @d@ periods, where @d@ is derived from the context.
--
-- @
-- delay2
--   :: HiddenClockResetEnable dom
--   => Int
--   -> 'DSignal' dom n Int
--   -> 'DSignal' dom (n + 2) Int
-- delay2 = 'delayedI'
-- @
--
-- >>> sampleN @System 7 (toSignal (delay2 (-1) (dfromList [0..])))
-- [-1,-1,-1,1,2,3,4]
--
-- Or @d@ can be specified using type application:
--
-- >>> :t delayedI @3
-- delayedI @3
--   :: (...
--       ...
--       ...
--       ...) =>
--      a -> DSignal dom n a -> DSignal dom (n + 3) a
delayedI
  :: ( KnownNat d
     , Undefined a
     , HiddenClockResetEnable dom  )
  => a
  -- ^ Initial value
  -> DSignal dom n a
  -> DSignal dom (n + d) a
delayedI = hideClockResetEnable E.delayedI

-- | Delay a 'DSignal' for @d@ cycles, the value at time 0..d-1 is /a/.
--
-- @
-- delay2
--   :: HiddenClockResetEnable dom
--   => Int
--   -> 'DSignal' dom n Int
--   -> 'DSignal' dom (n + 2) Int
-- delay2 = 'delayN' d2
-- @
--
-- >>> printX $ sampleN @System 6 (toSignal (delayN2 (-1) (dfromList [1..])))
-- [-1,-1,1,2,3,4]
delayN
  :: forall dom  a d n
   . ( HiddenClockResetEnable dom
     , Undefined a )
  => SNat d
  -> a
  -- ^ Initial value
  -> DSignal dom n a
  -> DSignal dom (n+d) a
delayN d dflt = coerce . go (snatToInteger d) . coerce @_ @(Signal dom a)
  where
    go 0 = id
    go i = delay dflt . go (i-1)

-- | Delay a 'DSignal' for @d@ cycles, where @d@ is derived from the context.
-- The value at time 0..d-1 is a default value.
--
-- @
-- delayI2
--   :: HiddenClockResetEnable dom
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
-- >>> sampleN @System 6 (toSignal (delayI @2 (-1) (dfromList [1..])))
-- [-1,-1,1,2,3,4]
delayI
  :: forall d n a dom
   . ( HiddenClockResetEnable dom
     , Undefined a
     , KnownNat d )
  => a
  -- ^ Initial value
  -> DSignal dom n a
  -> DSignal dom (n+d) a
delayI dflt = delayN (SNat :: SNat d) dflt

data DelayedFold (dom :: Domain) (n :: Nat) (delay :: Nat) (a :: Type) (f :: TyFun Nat Type) :: Type
type instance Apply (DelayedFold dom n delay a) k = DSignal dom (n + (delay*k)) a

-- | Tree fold over a 'Vec' of 'DSignal's with a combinatorial function,
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
   . ( HiddenClockResetEnable dom
     , Undefined a
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
delayedFold _ dflt op = dtfold (Proxy :: Proxy (DelayedFold dom n delay a)) id go
  where
    go :: SNat l
       -> DelayedFold dom n delay a @@ l
       -> DelayedFold dom n delay a @@ l
       -> DelayedFold dom n delay a @@ (l+1)
    go SNat x y = delayI dflt (op <$> x <*> y)
