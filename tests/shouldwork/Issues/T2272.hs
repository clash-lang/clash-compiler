{-# LANGUAGE CPP #-}
{-# LANGUAGE ImpredicativeTypes, TypeApplications, GADTs #-}
module T2272 where

import Clash.Prelude

data Ex where
  ExT :: (forall a . String -> a) -> Ex

f :: Ex
f = ExT (errorX @(forall b . String -> b) "qq")
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE f #-}

topEntity :: Int
topEntity = case f of
  ExT h -> h "kk"
