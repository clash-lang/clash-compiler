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
import           GHC.TypeLits                  (KnownNat, type (^), type (+), type (*), Nat)
import           Data.Singletons.Prelude       (Apply, TyFun, type (@@))

import Clash.Signal.Delayed.Internal
  (DSignal(..), dfromList, dfromList_lazy, fromSignal, toSignal,
   unsafeFromSignal, antiDelay, feedback)
import qualified Clash.Explicit.Signal.Delayed as E
import           Clash.Sized.Vector            (Vec, dtfold)
import           Clash.Signal
  (HiddenClockReset, hideClockReset, Signal, delay, Domain(..))

import           Clash.Promoted.Nat            (SNat (..), snatToInteger)
import           Clash.XException              (Undefined)

{- $setup
>>> :set -XDataKinds -XTypeOperators -XTypeApplications -XFlexibleContexts
>>> import Clash.Prelude
>>> let delay3 = delayed (-1 :> -1 :> -1 :> Nil)
>>> let delay2 = delayedI :: HiddenClockReset domain gated synchronous => Int -> DSignal domain n Int -> DSignal domain (n + 2) Int
>>> let delayN2 = delayN d2
>>> let delayI2 = delayI :: HiddenClockReset domain gated synchronous => Int -> DSignal domain n Int -> DSignal domain (n + 2) Int
>>> let countingSignals = Clash.Prelude.repeat (dfromList [0..]) :: Vec 4 (DSignal domain 0 Int)
-}

-- | Delay a 'DSignal' for @d@ periods.
--
-- @
-- delay3
--   :: HiddenClockReset domain gated synchronous
--   => 'DSignal' domain n Int
--   -> 'DSignal' domain (n + 3) Int
-- delay3 = 'delayed' (-1 ':>' -1 ':>' -1 ':>' 'Nil')
-- @
--
-- >>> sampleN 7 (toSignal (delay3 (dfromList [0..])))
-- [-1,-1,-1,-1,1,2,3]
delayed
  :: ( KnownNat d
     , HiddenClockReset domain gated synchronous
     , Undefined a
     )
  => Vec d a
  -> DSignal domain n a
  -> DSignal domain (n + d) a
delayed = hideClockReset E.delayed

-- | Delay a 'DSignal' for @d@ periods, where @d@ is derived from the context.
--
-- @
-- delay2
--   :: HiddenClockReset domain gated synchronous
--   => Int
--   -> 'DSignal' domain n Int
--   -> 'DSignal' domain (n + 2) Int
-- delay2 = 'delayedI'
-- @
--
-- >>> sampleN 7 (toSignal (delay2 (-1) (dfromList [0..])))
-- [-1,-1,-1,1,2,3,4]
--
-- Or @d@ can be specified using type application:
--
-- >>> :t delayedI @3
-- delayedI @3
--   :: (...
--       ...) =>
--      a -> DSignal domain n a -> DSignal domain (n + 3) a
delayedI
  :: ( KnownNat d
     , Undefined a
     , HiddenClockReset domain gated synchronous )
  => a
  -- ^ Default value
  -> DSignal domain n a
  -> DSignal domain (n + d) a
delayedI = hideClockReset E.delayedI

-- | Delay a 'DSignal' for @d@ cycles, the value at time 0..d-1 is /undefined/.
--
-- @
-- delay2
--   :: HiddenClockReset domain gated synchronous
--   => Int
--   -> 'DSignal' domain n Int
--   -> 'DSignal' domain (n + 2) Int
-- delay2 = 'delayN' d2
-- @
--
-- >>> printX $ sampleN 6 (toSignal (delayN2 (-1) (dfromList [1..])))
-- [-1,-1,1,2,3,4]
delayN
  :: forall domain gated synchronous a d n
   . ( HiddenClockReset domain gated synchronous
     , Undefined a )
  => SNat d
  -> a
  -- ^ Default value
  -> DSignal domain n a
  -> DSignal domain (n+d) a
delayN d dflt = coerce . go (snatToInteger d) . coerce @_ @(Signal domain a)
  where
    go 0 = id
    go i = delay dflt . go (i-1)

-- | Delay a 'DSignal' for @d@ cycles, where @d@ is derived from the context.
-- The value at time 0..d-1 is a default value.
--
-- @
-- delayI2
--   :: HiddenClockReset domain gated synchronous
--   => Int
--   -> 'DSignal' domain n Int
--   -> 'DSignal' domain (n + 2) Int
-- delayI2 = 'delayI'
-- @
--
-- >>> sampleN 6 (toSignal (delayI2 (-1) (dfromList [1..])))
-- [-1,-1,1,2,3,4]
--
-- You can also use type application to do the same:
-- >>> sampleN 6 (toSignal (delayI @2 (-1) (dfromList [1..])))
-- [-1,-1,1,2,3,4]
delayI
  :: forall d n a domain gated synchronous
   . ( HiddenClockReset domain gated synchronous
     , Undefined a
     , KnownNat d )
  => a
  -- ^ Default value
  -> DSignal domain n a
  -> DSignal domain (n+d) a
delayI dflt = delayN (SNat :: SNat d) dflt

data DelayedFold (domain :: Domain) (n :: Nat) (delay :: Nat) (a :: Type) (f :: TyFun Nat Type) :: Type
type instance Apply (DelayedFold domain n delay a) k = DSignal domain (n + (delay*k)) a

-- | Tree fold over a 'Vec' of 'DSignal's with a combinatorial function,
-- and delaying @delay@ cycles after each application.
-- Values at times 0..(delay*k)-1 are set to a default.
--
-- @
-- countingSignals :: Vec 4 (DSignal domain 0 Int)
-- countingSignals = repeat (dfromList [0..])
-- @
--
-- >>> printX $ sampleN 6 (toSignal (delayedFold d1 (-1) (+) countingSignals))
-- [-1,-2,0,4,8,12]
--
-- >>> printX $ sampleN 8 (toSignal (delayedFold d2 (-1) (*) countingSignals))
-- [-1,-1,1,1,0,1,16,81]
delayedFold
  :: forall domain gated synchronous n delay k a
   . ( HiddenClockReset domain gated synchronous
     , Undefined a
     , KnownNat delay
     , KnownNat k )
  => SNat delay
  -- ^ Delay applied after each step
  -> a
  -- ^ Default value
  -> (a -> a -> a)
  -- ^ Fold operation to apply
  -> Vec (2^k) (DSignal domain n a)
  -- ^ Vector input of size 2^k
  -> DSignal domain (n + (delay * k)) a
  -- ^ Output Signal delayed by (delay * k)
delayedFold _ dflt op = dtfold (Proxy :: Proxy (DelayedFold domain n delay a)) id go
  where
    go :: SNat l
       -> DelayedFold domain n delay a @@ l
       -> DelayedFold domain n delay a @@ l
       -> DelayedFold domain n delay a @@ (l+1)
    go SNat x y = delayI dflt (op <$> x <*> y)
