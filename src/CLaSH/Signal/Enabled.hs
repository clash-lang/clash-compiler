{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE TupleSections #-}
module CLaSH.Signal.Enabled
  ( Enabled
  , regEn
  , enabled
  , mapEnabled
  , zipWithEnabled
  )
where

import Control.Applicative   (liftA2)
import Control.Arrow         (second)

import CLaSH.Signal.Internal ((&&$), mux, register#)
import CLaSH.Signal.Explicit (CSignal, SClock)
import CLaSH.Signal.Bundle   (Bundle (..))

type Enabled a = (Bool, a)

regEn :: SClock clk
      -> a
      -> CSignal clk (Enabled a)
      -> CSignal clk a
regEn clk is s = r
  where
    (en,a) = unbundle clk s
    r      = regEn# clk is en a

{-# NOINLINE regEn# #-}
regEn# :: SClock clk -> a -> CSignal clk Bool -> CSignal clk a -> CSignal clk a
regEn# clk is en s = r
  where
    r = register# clk is d
    d = mux en s r

enabled :: CSignal clk a
        -> CSignal clk (Enabled a)
enabled = fmap (True,)

mapEnabled :: (a -> b)
           -> CSignal clk (Enabled a)
           -> CSignal clk (Enabled b)
mapEnabled f = fmap (second f)

zipWithEnabled :: SClock clk
               -> (a -> b -> c)
               -> CSignal clk (Enabled a)
               -> CSignal clk (Enabled b)
               -> CSignal clk (Enabled c)
zipWithEnabled clk f l r = bundle clk (b1 &&$ b2, liftA2 f l' r')
  where
    (b1,l') = unbundle clk l
    (b2,r') = unbundle clk r
