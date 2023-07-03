{-# LANGUAGE CPP #-}

module FindIndex where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: Vec 7 (Unsigned 8) -> Maybe (Index 7)
topEntity = findIndex (> 3)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = pure (1:>3:>2:>4:>3:>5:>6:>Nil)
    expectedOutput = outputVerifier' clk rst ((Just 3) :> Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
