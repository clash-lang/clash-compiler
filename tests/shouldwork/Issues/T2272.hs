{-# LANGUAGE ImpredicativeTypes, TypeApplications, GADTs #-}
module T2272 where

import Clash.Prelude

data Ex where
  ExT :: (forall a . String -> a) -> Ex

f :: Ex
f = ExT (errorX @(forall b . String -> b) "qq")
{-# NOINLINE f #-}

topEntity :: Int
topEntity = case f of
  ExT h -> h "kk"
