module T1354A where

import Clash.Prelude

-- Clash should be able to compile this without getting stuck in an infinite
-- loop. See https://github.com/clash-lang/clash-compiler/pull/1354#issuecomment-635430374

f :: forall n. KnownNat n => Index (n + 1) -> Int -> Int
f n = foldl (.) id lanes
 where
  lanes :: Vec (n + 1) (Int -> Int)
  lanes = map (\i -> if i < n then id else id) (iterateI succ 0)

topEntity :: Int -> Int
topEntity = f (1 :: Index 3)
