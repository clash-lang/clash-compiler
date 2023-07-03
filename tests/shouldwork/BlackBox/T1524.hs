{-# LANGUAGE CPP #-}

module T1524 where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: Vec 2 (Vec 3 Int) -> Vec 2 Int
topEntity = f @2 @(3-1)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

f :: Vec k (Vec (l+1) Int) -> Vec k Int
f input = map g input

g :: Vec (l+1) Int -> Int
g = fold (+)

case1 :: Vec 2 (Vec 3 Int)
case1 = (1 :> 2 :> 3 :> Nil) :> (10 :> 20 :> 30 :> Nil) :> Nil
case2 = (-1 :> -2 :> -3 :> Nil) :> (10 :> -20 :> 30 :> Nil) :> Nil

input = case1 :> case2 :> Nil
expected = (6 :> 60 :> Nil) :> (-6 :> 20 :> Nil) :> Nil

testBench :: Signal System Bool
testBench = done
 where
  done = outputVerifier' clk aclr expected (topEntity <$> inp)
  clk  = tbSystemClockGen (not <$> done)
  inp  = stimuliGenerator clk aclr input
  aclr = systemResetGen
