{-# LANGUAGE CPP #-}

module VMerge where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: (Vec 2 Int,Vec 2 Int) -> Vec 4 Int
topEntity (x,y) = merge x y
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = pure (iterateI (+1) 1,iterateI (+1) 3)
    expectedOutput = outputVerifier' clk rst ((1:>3:>2:>4:>Nil):>Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
