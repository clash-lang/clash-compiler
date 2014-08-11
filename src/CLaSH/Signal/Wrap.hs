{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TypeFamilies      #-}
module CLaSH.Signal.Wrap
  ( Wrap (..)
  )
where

import Control.Applicative   (liftA2)
import Prelude               hiding (head, map, tail)

import CLaSH.Signal.Internal (Clock, CSignal (..), SClock)
import CLaSH.Sized.Vector    (Vec (..), head, map, tail)

-- | Isomorphism between a 'CSignal' of a product type (e.g. a tuple) and a
-- product type of 'CSignal's.
--
-- Instances of 'Wrap' must satisfy the following laws:
--
-- @
-- unwrap . wrap = 'id'
-- wrap . unwrap = 'id'
-- @
class Wrap a where
  type Wrapped (clk :: Clock) a
  type Wrapped clk a = CSignal clk a
  -- | Example:
  --
  -- > unwrap :: (CSignal clk a, CSignal clk b) -> CSignal clk (a,b)
  --
  -- However:
  --
  -- > unwrap :: CSignal clk Bit -> Signal clk Bit
  unwrap :: SClock clk -> Wrapped clk a -> CSignal clk a

  default unwrap :: SClock clk ->  CSignal clk a -> CSignal clk a
  unwrap _ s = s
  -- | Example:
  --
  -- > wrap :: CSignal clk (a,b) -> (Signal clk a, Signal clk b)
  --
  -- However:
  --
  -- > wrap :: Signal Bit -> Signal Bit
  wrap :: SClock clk -> CSignal clk a -> Wrapped clk a

  default wrap :: SClock clk -> CSignal clk a -> CSignal clk a
  wrap _ s = s

instance Wrap Bool
instance Wrap Integer
instance Wrap Int
instance Wrap Float
instance Wrap Double
instance Wrap ()

instance Wrap (a,b) where
  type Wrapped t (a,b) = (CSignal t a, CSignal t b)
  unwrap _     = uncurry (liftA2 (,))
  wrap   _ tup = (fmap fst tup, fmap snd tup)

instance Wrap (Vec n a) where
  type Wrapped t (Vec n a) = Vec n (CSignal t a)
  unwrap clk vs = (map shead vs) :- (unwrap clk (map stail vs))
    where
      shead (s :- _)  = s
      stail (_ :- ss) = ss

  wrap _      (Nil :- _)      = Nil
  wrap clk vs@((_ :> _) :- _) = fmap head vs :> wrap clk (fmap tail vs)
