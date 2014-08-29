{-# LANGUAGE TupleSections #-}
module CLaSH.Signal.Enabled
  ( Enabled
  , regEn
  , enabled
  , mapEnabled
  , zipWithEnabled
  )
where

import Control.Applicative   (Applicative (..), (<$>), liftA2)
import Control.Arrow         (second)

import CLaSH.Signal.Internal ((&&$),mux)
import CLaSH.Signal.Explicit (CSignal, SClock, cregister)
import CLaSH.Signal.Wrap     (Wrap (..))

type Enabled a = (Bool, a)

regEn :: SClock clk
      -> a
      -> CSignal clk (Enabled a)
      -> CSignal clk a
regEn clk is s = r
  where
    r     = cregister clk is d
    (v,a) = wrap clk s
    d     = mux v a r

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
zipWithEnabled clk f l r = unwrap clk (b1 &&$ b2, liftA2 f l' r')
  where
    (b1,l') = wrap clk l
    (b2,r') = wrap clk r
