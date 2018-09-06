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

import Control.DeepSeq            (NFData)
import Data.Coerce                (coerce)
import Data.Default.Class         (Default(..))
import GHC.TypeLits               (KnownNat, Nat, type (+))
import Language.Haskell.TH.Syntax (Lift)
import Prelude                    hiding (head, length, repeat)
import Test.QuickCheck            (Arbitrary, CoArbitrary)

import Clash.Promoted.Nat         (SNat)
import Clash.Sized.Vector
  (Vec, head, length, repeat, shiftInAt0, singleton)
import Clash.Explicit.Signal
  (Clock, Domain, Reset, Signal, register, fromList, fromList_lazy, bundle,
   unbundle)
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

-- | A synchronized signal with samples of type @a@, synchronized to clock
-- @clk@, that has accumulated @delay@ amount of samples delay along its path.
newtype DSignal (domain :: Domain) (delay :: Nat) a =
    DSignal { -- | Strip a 'DSignal' from its delay information.
              toSignal :: Signal domain a
            }
  deriving (Show,Default,Functor,Applicative,Num,Fractional,
            Foldable,Traversable,Arbitrary,CoArbitrary,Lift)

-- | Create a 'DSignal' from a list
--
-- Every element in the list will correspond to a value of the signal for one
-- clock cycle.
--
-- >>> sampleN 2 (dfromList [1,2,3,4,5])
-- [1,2]
--
-- __NB__: This function is not synthesisable
dfromList :: NFData a => [a] -> DSignal domain 0 a
dfromList = coerce . fromList

-- | Create a 'DSignal' from a list
--
-- Every element in the list will correspond to a value of the signal for one
-- clock cycle.
--
-- >>> sampleN 2 (dfromList [1,2,3,4,5])
-- [1,2]
--
-- __NB__: This function is not synthesisable
dfromList_lazy :: [a] -> DSignal domain 0 a
dfromList_lazy = coerce . fromList_lazy

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

-- | Feed the delayed result of a function back to its input:
--
-- @
-- mac :: Clock domain gated -> Reset domain synchronous
--     -> 'DSignal' domain 0 Int -> 'DSignal' domain 0 Int -> 'DSignal' domain 0 Int
-- mac clk rst x y = 'feedback' (mac' x y)
--   where
--     mac' :: 'DSignal' domain 0 Int -> 'DSignal' domain 0 Int -> 'DSignal' domain 0 Int
--          -> ('DSignal' domain 0 Int, 'DSignal' domain 1 Int)
--     mac' a b acc = let acc' = a * b + acc
--                    in  (acc, 'delay' clk rst ('singleton' 0) acc')
-- @
--
-- >>> sampleN 6 (mac systemClockGen systemResetGen (dfromList [1..]) (dfromList [1..]))
-- [0,1,5,14,30,55]
feedback
  :: (DSignal domain n a -> (DSignal domain n a,DSignal domain (n + m + 1) a))
  -> DSignal domain n a
feedback f = let (o,r) = f (coerce r) in o

-- | 'Signal's are not delayed
--
-- > sample s == dsample (fromSignal s)
fromSignal :: Signal domain a -> DSignal domain 0 a
fromSignal = coerce

-- | __EXPERIMENTAL__
--
-- __Unsafely__ convert a 'Signal' to /any/ 'DSignal' clk'.
--
-- __NB__: Should only be used to interface with functions specified in terms of
-- 'Signal'.
unsafeFromSignal :: Signal domain a -> DSignal domain n a
unsafeFromSignal = DSignal

-- | __EXPERIMENTAL__
--
-- Access a /delayed/ signal in the present.
--
-- @
-- mac :: Clock domain gated -> Reset domain synchronous
--     -> 'DSignal' domain 0 Int -> 'DSignal' domain 0 Int -> 'DSignal' domain 0 Int
-- mac clk rst x y = acc'
--   where
--     acc' = (x * y) + 'antiDelay' d1 acc
--     acc  = 'delay' clk rst ('singleton' 0) acc'
-- @
antiDelay :: SNat d -> DSignal domain (n + d) a -> DSignal domain n a
antiDelay _ = coerce
