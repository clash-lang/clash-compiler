{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017     , Google Inc.
                  2019     , Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
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
    -- * List \<-\> DSignal conversion (not synthesizable)
  , dfromList
    -- ** lazy versions
  , dfromList_lazy
    -- * Experimental
  , unsafeFromSignal
  , antiDelay
  )
where

import Data.Coerce                (coerce)
import GHC.TypeLits               (KnownNat, type (+))
import Prelude                    hiding (head, length, repeat)

import Clash.Sized.Vector
  (Vec, head, length, repeat, shiftInAt0, singleton)
import Clash.Signal.Delayed.Internal
  (DSignal(..), dfromList, dfromList_lazy, fromSignal, toSignal,
   unsafeFromSignal, antiDelay, feedback)

import Clash.Explicit.Signal
  (KnownDomain, Clock, Reset, Signal, Enable, register,  bundle, unbundle)

import Clash.XException           (Undefined)

{- $setup
>>> :set -XDataKinds
>>> :set -XTypeOperators
>>> import Clash.Explicit.Prelude
>>> let delay3 clk rst en = delayed clk rst en (-1 :> -1 :> -1 :> Nil)
>>> let delay2 clk rst en = (delayedI clk rst en :: Int -> DSignal System n Int -> DSignal System (n + 2) Int)
>>> :{
let mac :: Clock System
        -> Reset System
        -> Enable System
        -> DSignal System 0 Int -> DSignal System 0 Int
        -> DSignal System 0 Int
    mac clk rst en x y = feedback (mac' x y)
      where
        mac' :: DSignal System 0 Int -> DSignal System 0 Int
             -> DSignal System 0 Int
             -> (DSignal System 0 Int, DSignal System 1 Int)
        mac' a b acc = let acc' = a * b + acc
                       in  (acc, delayed clk rst en (singleton 0) acc')
:}

-}

-- TODO: Reimplement with dtfold
-- | Delay a 'DSignal' for @d@ periods.
--
-- @
-- delay3
--   :: Clock dom
--   -> Reset dom
--   -> Enable dom
--   -> 'DSignal' dom n Int
--   -> 'DSignal' dom (n + 3) Int
-- delay3 clk rst en = 'delayed' clk rst en (-1 ':>' -1 ':>' -1 ':>' 'Nil')
-- @
--
-- >>> sampleN 7 (delay3 systemClockGen resetGen enableGen (dfromList [0..]))
-- [-1,-1,-1,-1,1,2,3]
delayed
  :: forall dom  a n d
   . ( KnownDomain dom
     , KnownNat d
     , Undefined a )
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> Vec d a
  -- ^ Initial values
  -> DSignal dom n a
  -> DSignal dom (n + d) a
delayed clk rst en m ds = coerce (delaySignal (coerce ds))
  where
    delaySignal :: Signal dom a -> Signal dom a
    delaySignal s = case length m of
      0 -> s
      _ -> let (r',o) = shiftInAt0 (unbundle r) (singleton s)
               r      = register clk rst en m (bundle r')
           in  head o

-- | Delay a 'DSignal' for @d@ periods, where @d@ is derived from the
-- context.
--
-- @
-- delay2
--   :: Clock dom
--   -> Reset dom
--   -> Enable dom
--   -> Int
--   -> 'DSignal' dom n Int
--   -> 'DSignal' dom (n + 2) Int
-- delay2 = 'delayedI'
-- @
--
-- >>> sampleN 7 (delay2 systemClockGen resetGen enableGen (-1) (dfromList ([0..])))
-- [-1,-1,-1,1,2,3,4]
--
-- @d@ can also be specified using type application:
--
-- >>> :t delayedI @3
-- delayedI @3
--   :: ... =>
--      Clock dom
--      -> Reset dom
--      -> Enable dom
--      -> a
--      -> DSignal dom n a
--      -> DSignal dom (n + 3) a
delayedI
  :: ( KnownNat d
     , KnownDomain dom
     , Undefined a )
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> a
  -- ^ Initial value
  -> DSignal dom n a
  -> DSignal dom (n + d) a
delayedI clk rst en dflt = delayed clk rst en (repeat dflt)
