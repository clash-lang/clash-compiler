module TwoFunctions where

import Clash.Prelude

f :: Bool -> a -> a -> a
f True  x y = x
f False x y = y

g :: Bool -> b -> b -> b
g True c d  = d
g False c d = c

topEntity :: Bool -> Int -> Int -> (Int,Int)
topEntity t a b = (f t a b, g t a b)
