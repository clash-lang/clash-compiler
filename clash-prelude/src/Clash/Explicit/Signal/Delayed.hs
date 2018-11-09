{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017     , Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Explicit.Signal.Delayed
  ( DSignal
    -- * Delay-annotated synchronous signals
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

import Data.Coerce                (coerce)
import Data.Default.Class         (Default(..))
import GHC.TypeLits               (KnownNat, type (+))
import Prelude                    hiding (head, length, repeat)

import Clash.Sized.Vector
  (Vec, head, length, repeat, shiftInAt0, singleton)
import Clash.Signal.Delayed.Internal
  (DSignal(..), dfromList, dfromList_lazy, fromSignal, toSignal,
   unsafeFromSignal, antiDelay, feedback)

import Clash.Explicit.Signal
  (Clock, Reset, Signal, register,  bundle, unbundle)
import Clash.XException           (Undefined)

{- $setup
>>> :set -XDataKinds
>>> :set -XTypeOperators
>>> import Clash.Explicit.Prelude
>>> let delay3 clk rst = delayed clk rst (0 :> 0 :> 0 :> Nil)
>>> let delay2 clk rst = (delayedI clk rst :: DSignal System n Int -> DSignal System (n + 2) Int)
>>> :{
let mac :: Clock System gated
        -> Reset System synchronous
        -> DSignal System 0 Int -> DSignal System 0 Int
        -> DSignal System 0 Int
    mac clk rst x y = feedback (mac' x y)
      where
        mac' :: DSignal System 0 Int -> DSignal System 0 Int
             -> DSignal System 0 Int
             -> (DSignal System 0 Int, DSignal System 1 Int)
        mac' a b acc = let acc' = a * b + acc
                       in  (acc, delayed clk rst (singleton 0) acc')
:}

-}

-- | Delay a 'DSignal' for @d@ periods.
--
-- @
-- delay3 :: Clock domain gated -> Reset domain synchronous
--        -> 'DSignal' domain n Int -> 'DSignal' domain (n + 3) Int
-- delay3 clk rst = 'delayed' clk rst (0 ':>' 0 ':>' 0 ':>' 'Nil')
-- @
--
-- >>> sampleN 6 (delay3 systemClockGen systemResetGen (dfromList [1..]))
-- [0,0,0,1,2,3]
delayed
  :: forall domain gated synchronous a n d
   . (KnownNat d, Undefined a)
  => Clock domain gated
  -> Reset domain synchronous
  -> Vec d a
  -> DSignal domain n a
  -> DSignal domain (n + d) a
delayed clk rst m ds = coerce (delaySignal (coerce ds))
  where
    delaySignal :: Signal domain a -> Signal domain a
    delaySignal s = case length m of
      0 -> s
      _ -> let (r',o) = shiftInAt0 (unbundle r) (singleton s)
               r      = register clk rst m (bundle r')
           in  head o

-- | Delay a 'DSignal' for @m@ periods, where @m@ is derived from the
-- context.
--
-- @
-- delay2 :: Clock domain gated -> Reset domain synchronous
--        -> 'DSignal' domain n Int -> 'DSignal' domain (n + 2) Int
-- delay2 = 'delayI'
-- @
--
-- >>> sampleN 6 (delay2 systemClockGen systemResetGen (dfromList [1..]))
-- [0,0,1,2,3,4]
delayedI
  :: (Default a, KnownNat d, Undefined a)
  => Clock domain gated
  -> Reset domain synchronous
  -> DSignal domain n a
  -> DSignal domain (n + d) a
delayedI clk rst = delayed clk rst (repeat def)
