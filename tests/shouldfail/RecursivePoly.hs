{-# LANGUAGE ScopedTypeVariables #-}
module RecursivePoly where

import Clash.Prelude

topEntity :: Integer
topEntity = f 0

f :: (Integral b, Num b) => b -> b
f x = let
        f1 :: Num a => a -> a
        f1 = \(y::a) -> (f1 y) + 1 + (fromInteger $ toInteger x)
      in
        f1 x
