{-# LANGUAGE CPP #-}

module Scatter where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: Vec 5 (Unsigned 10) -> Vec 5 (Unsigned 10)
topEntity = scatter defvec to
  where
    defvec = replicate d5 99
    to = 0 :> 4 :> 2 :> 3 :> 1 :> Nil
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst ((1 :> 2 :> 3 :> 4 :> 5 :> Nil) :> Nil)
    expectedOutput = outputVerifier'   clk rst ((1 :> 5 :> 3 :> 4 :> 2 :> Nil) :> Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
