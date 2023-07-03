{-# LANGUAGE CPP #-}

module Fold where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: Vec 8 Int -> Int
topEntity = fold (+)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = pure (1:>2:>3:>4:>5:>6:>7:>8:>Nil)
    expectedOutput = outputVerifier' clk rst (36 :> Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
