module Iterate where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: Int -> Vec 2 Int
topEntity = iterateI succ
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  testInput      = stimuliGenerator clk rst (3 :> 5 :> Nil)
  expectedOutput = outputVerifier' clk rst ((3 :> 4 :> Nil) :> (5 :> 6 :> Nil) :> Nil)
  done           = expectedOutput (topEntity <$> testInput)
  clk            = tbSystemClockGen (not <$> done)
  rst            = systemResetGen
