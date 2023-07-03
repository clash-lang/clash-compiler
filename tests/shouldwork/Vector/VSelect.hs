{-# LANGUAGE CPP #-}

module VSelect where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: Vec 8 Int -> Vec 4 Int
topEntity x = select d1 d2 d4 x
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = pure (iterateI (+1) 1)
    expectedOutput = outputVerifier' clk rst ((2:>4:>6:>8:>Nil):>Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
