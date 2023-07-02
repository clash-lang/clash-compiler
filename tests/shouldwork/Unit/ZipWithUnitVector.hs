{-# LANGUAGE CPP #-}

module ZipWithUnitVector where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity
  :: Vec 2 (Int,Int)
  -> Vec 2 ((Int,Int),())
topEntity xs = zipWith (,) xs (repeat ())
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst (repeat (2, 2) :> repeat (3, 4) :> Nil)
    expectedOutput = outputVerifier' clk rst (repeat ((2, 2), ()) :> repeat ((3, 4), ()) :> Nil)

    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
