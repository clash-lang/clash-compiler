{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017     , Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType #-}
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
    -- * List \<-\> DSignal conversion (not synthesisable)
  , dfromList
    -- ** lazy versions
  , dfromList_lazy
    -- * Experimental
  , unsafeFromSignal
  , antiDelay
  )
where

import           Data.Default.Class            (Default)
import           Data.Coerce                   (coerce)
import           Data.Kind                     (Type)
import           Data.Proxy                    (Proxy(..))
import           GHC.TypeLits                  (KnownNat, type (^), type (+), type (*), Nat)
import           Data.Singletons.Prelude       (Apply, TyFun, type (@@))

import Clash.Signal.Delayed.Internal
  (DSignal(..), dfromList, dfromList_lazy, fromSignal, toSignal,
   unsafeFromSignal, antiDelay, feedback)
import qualified Clash.Explicit.Signal.Delayed as E
import            Clash.Sized.Vector           (Vec, dtfold)
import            Clash.Signal
  (HiddenClockReset, hideClockReset, Signal, delay, Domain(..))
import            Clash.XException

import Clash.Promoted.Nat         (SNat (..), snatToInteger)

{- $setup
>>> :set -XDataKinds -XTypeOperators -XTypeApplications -XFlexibleContexts
>>> import Clash.Prelude
>>> let delay3 = delayed (0 :> 0 :> 0 :> Nil)
>>> let delay2 = delayedI :: HiddenClockReset domain gated synchronous => DSignal domain n Int -> DSignal domain (n + 2) Int
>>> let delayN2 = delayN (SNat :: SNat 2)
>>> let delayI2 = delayI :: HiddenClockReset domain gated synchronous => DSignal domain n Int -> DSignal domain (n + 2) Int
>>> let countingSignals = Clash.Prelude.repeat (dfromList [0..]) :: Vec 4 (DSignal domain 0 Int)
-}

-- | Delay a 'DSignal' for @d@ periods.
--
-- @
-- delay3 :: HiddenClockReset domain gated synchronous =>
--          'DSignal' domain n Int -> 'DSignal' domain (n + 3) Int
-- delay3 = 'delayed' (0 ':>' 0 ':>' 0 ':>' 'Nil')
-- @
--
-- >>> sampleN 7 (toSignal (delay3 (dfromList [0..])))
-- [0,0,0,0,1,2,3]
delayed
  :: (KnownNat d, Undefined a, HiddenClockReset domain gated synchronous)
  => Vec d a
  -> DSignal domain n a
  -> DSignal domain (n + d) a
delayed = hideClockReset E.delayed

-- | Delay a 'DSignal' for @m@ periods, where @m@ is derived from the context.
--
-- @
-- delay2 :: HiddenClockReset domain gated synchronous =>
--          'DSignal' domain n Int -> 'DSignal' domain (n + 2) Int
-- delay2 = 'delayedI'
-- @
--
-- >>> sampleN 7 (toSignal (delay2 (dfromList [0..])))
-- [0,0,0,1,2,3,4]
delayedI
  :: (Default a, KnownNat d, Undefined a, HiddenClockReset domain gated synchronous)
  => DSignal domain n a
  -> DSignal domain (n + d) a
delayedI = hideClockReset E.delayedI

-- | Delay a 'DSignal' for @d@ cycles, the value at time 0..d-1 is /undefined/.
--
-- @
-- delay2 :: HiddenClockReset domain gated synchronous =>
--          'DSignal' domain n Int -> 'DSignal' domain (n + 2) Int
-- delay2 = 'delayN' (SNat :: SNat 2)
-- @
--
-- >>> printX $ sampleN 6 (toSignal (delayN2 (dfromList [1..])))
-- [X,X,1,2,3,4]
delayN :: forall domain gated synchronous a d n .
          (HiddenClockReset domain gated synchronous, Undefined a)
       => SNat d
       -> DSignal domain n a
       -> DSignal domain (n+d) a
delayN d = coerce . go (snatToInteger d) . coerce @_ @(Signal domain a)
  where
    go 0 = id
    go i = delay . go (i-1)

-- | Delay a 'DSignal' for @d@ cycles, where @d@ is derived from the context.
-- The value at time 0..d-1 is /undefined/.
--
-- @
-- delay2 :: HiddenClockReset domain gated synchronous =>
--          'DSignal' domain n Int -> 'DSignal' domain (n + 2) Int
-- delay2 = 'delayI'
-- @
--
-- >>> printX $ sampleN 6 (toSignal (delayI2 (dfromList [1..])))
-- [X,X,1,2,3,4]
delayI :: forall domain gated synchronous d n a.
          (HiddenClockReset domain gated synchronous, KnownNat d, Undefined a)
          => DSignal domain n a
          -> DSignal domain (n+d) a
delayI = delayN (SNat :: SNat d)

data DelayedFold (domain :: Domain) (n :: Nat) (delay :: Nat) (a :: Type) (f :: TyFun Nat Type) :: Type
type instance Apply (DelayedFold domain n delay a) k = DSignal domain (n + (delay*k)) a

-- | Tree fold over a 'Vec' of 'DSignal's with a combinatorial function,
-- and delaying @delay@ cycles after each application.
-- Values at times 0..(delay*k)-1 are /undefined/ after a reset.
--
-- @
-- countingSignals :: Vec 4 (DSignal domain 0 Int)
-- countingSignals = repeat (dfromList [0..])
-- @
--
-- >>> printX $ sampleN 6 (toSignal (delayedFold d1 (+) countingSignals))
-- [X,X,0,4,8,12]
--
-- >>> printX $ sampleN 8 (toSignal (delayedFold d2 (*) countingSignals))
-- [X,X,X,X,0,1,16,81]
delayedFold :: forall domain gated synchronous n delay k a.
                ( HiddenClockReset domain gated synchronous
                , KnownNat delay
                , KnownNat k
                , Undefined a)
               => SNat delay                         -- ^ Delay applied after each step
               -> (a -> a -> a)                      -- ^ Fold operation to apply
               -> Vec (2^k) (DSignal domain n a)     -- ^ Vector input of size 2^k
               -> DSignal domain (n + (delay * k)) a -- ^ Output Signal delayed by (delay * k)
delayedFold _ op = dtfold (Proxy :: Proxy (DelayedFold domain n delay a)) id go
  where
    go :: SNat l
       -> DelayedFold domain n delay a @@ l
       -> DelayedFold domain n delay a @@ l
       -> DelayedFold domain n delay a @@ (l+1)
    go SNat x y = delayI (op <$> x <*> y)
