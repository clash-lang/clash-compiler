{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Iterate where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: Int -> Vec 2 Int
topEntity = iterateI succ
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  output         = $(lift (map (iterate d2 (succ @Int)) (333 :> 444 :> Nil)))
  testInput      = stimuliGenerator clk rst (333 :> 444 :> Nil)
  expectedOutput = outputVerifier' clk rst output
  done           = expectedOutput (topEntity <$> testInput)
  clk            = tbSystemClockGen (not <$> done)
  rst            = systemResetGen
