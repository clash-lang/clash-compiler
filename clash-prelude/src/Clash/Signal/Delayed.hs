{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017     , Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-# LANGUAGE Safe #-}

module Clash.Signal.Delayed
  ( -- * Delay-annotated synchronous signals
    DSignal
  , delayed
  , delayedI
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
import           GHC.TypeLits                  (KnownNat, type (+))

import qualified Clash.Explicit.Signal.Delayed as E
import           Clash.Explicit.Signal.Delayed
  (DSignal, dfromList, dfromList_lazy, feedback, fromSignal, toSignal,
   unsafeFromSignal, antiDelay)
import            Clash.Sized.Vector           (Vec)
import            Clash.Signal
  (HiddenClockReset, hideClockReset)
import            Clash.XException

{- $setup
>>> :set -XDataKinds -XTypeOperators -XTypeApplications -XFlexibleContexts
>>> import Clash.Prelude
>>> let delay3 = delayed (0 :> 0 :> 0 :> Nil)
>>> let delay2 = delayedI :: HiddenClockReset domain gated synchronous => DSignal domain n Int -> DSignal domain (n + 2) Int
-}

-- | Delay a 'DSignal' for @d@ periods.
--
-- @
-- delay3 :: 'DSignal' n Int -> 'DSignal' (n + 3) Int
-- delay3 = 'delayed' (0 ':>' 0 ':>' 0 ':>' 'Nil')
-- @
--
-- >>> sampleN 6 (toSignal (delay3 (dfromList [1..])))
-- [0,0,0,1,2,3]
delayed
  :: (KnownNat d, Undefined a, HiddenClockReset domain gated synchronous)
  => Vec d a
  -> DSignal domain n a
  -> DSignal domain (n + d) a
delayed = hideClockReset E.delayed

-- | Delay a 'DSignal' for @m@ periods, where @m@ is derived from the context.
--
-- @
-- delay2 :: 'DSignal' n Int -> 'DSignal' (n + 2) Int
-- delay2 = 'delayedI'
-- @
--
-- >>> sampleN 6 (toSignal (delay2 (dfromList [1..])))
-- [0,0,1,2,3,4]
delayedI
  :: (Default a, KnownNat d, Undefined a, HiddenClockReset domain gated synchronous)
  => DSignal domain n a
  -> DSignal domain (n + d) a
delayedI = hideClockReset E.delayedI
