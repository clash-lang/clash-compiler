module TwoFunctions where

f :: Bool -> a -> a -> a
f True  x y = x
f False x y = y

g :: Bool -> b -> b -> b
g True c d  = d
g False c d = c
