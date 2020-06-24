{-# OPTIONS_GHC -O #-}
module T1402 where

import Clash.Prelude

topEntity :: Maybe (BitVector 128) -> BitVector 128 -> (BitVector 128,Bool)
topEntity q y = case q of
  Just b -> f True b
  _      -> f False y

f :: Bool -> BitVector 128 ->  (BitVector 128, Bool)
f t x = case x of
  1 -> (x,t)
  _ -> (x,t)