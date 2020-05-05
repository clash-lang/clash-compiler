{-# LANGUAGE GADTs #-}
module T1310 where

import Clash.Prelude

data Ex where
  Ex :: forall n . SNat n -> Bool -> Ex

f :: Ex -> Bool
f (Ex n y) = h (replicate n y)
{-# NOINLINE f #-}

h :: Vec n Bool -> Bool
h xs = foldr (||) True xs
{-# NOINLINE h #-}

g :: Int -> Bool -> Ex
g 0 b = Ex (SNat @3) b
g n b = g (n-1) b

topEntity :: Bool -> Bool
topEntity b = f (g 1 b)
