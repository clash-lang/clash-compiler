{-# LANGUAGE CPP #-}

module SFixedTest where

import Clash.Prelude
import Clash.Explicit.Testbench

type SF = SFixed 4 18

topEntity :: SF -> SF
topEntity x = x * 2.56
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst $(listToVecTH ([1.2, 1.8, 3.5] :: [SFixed 4 18] ))
    expectedOutput = outputVerifier'   clk rst $(listToVecTH ([3.07199, 4.607991, 8.96] :: [SFixed 4 18]))
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
