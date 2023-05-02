module SimOnly where

import Clash.Explicit.Prelude
import qualified Data.Map as Map

type Ignore = SimOnly (Map.Map String [Int])

unionIgnore :: Ignore -> Ignore -> Ignore
unionIgnore (SimOnly m1) (SimOnly m2) = SimOnly (Map.unionWith (<>) m1 m2)

foo :: Int -> Int -> (Int, Ignore)
foo a b = (a+b,SimOnly (Map.fromList [("foo",[a + b])]))
{-# NOINLINE foo #-}

bar :: Int -> Int -> (Int, Ignore)
bar a b = (a*b,SimOnly (Map.fromList [("bar",[a * b])]))

topEntity :: Int -> Int -> (Int, Ignore)
topEntity a b =
  let (z,np1) = foo a b
      (y,np2) = bar a b
   in (y - z, unionIgnore np1 np2)
