{-# LANGUAGE CPP #-}

module Transpose where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity = transposeV
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

transposeV :: Vec 3 (Vec 4 Int) -> Vec 4 (Vec 3 Int)
transposeV = sequenceA

testBench :: Signal System Bool
testBench = done
  where
    testInput      = pure ((1:>2:>3:>4:>Nil):>(5:>6:>7:>8:>Nil):>(9:>10:>11:>12:>Nil):>Nil)
    expectedOutput = outputVerifier' clk rst ((transpose ((1:>2:>3:>4:>Nil):>(5:>6:>7:>8:>Nil):>(9:>10:>11:>12:>Nil):>Nil)):>Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
