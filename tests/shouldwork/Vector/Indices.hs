{-# LANGUAGE CPP #-}

module Indices where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

topEntity
  :: Vec 2 (Index 2)
  -> Vec 2 (Index 3)
topEntity input = liftA2 add (indices SNat) input
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier'   clk aclr ((0 :> 2 :> Nil) :> Nil)
    testInput      = stimuliGenerator clk aclr ((0 :> 1 :> Nil) :> Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    aclr           = systemResetGen
