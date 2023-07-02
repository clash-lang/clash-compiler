{-# LANGUAGE CPP #-}

module VRotate where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: Vec 5 Int -> (Vec 5 Int,Vec 5 Int)
topEntity v = (rotateLeftS v d2,rotateRightS v d2)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = pure (1:>2:>3:>4:>5:>Nil)
    expectedOutput = outputVerifier' clk rst ((3:>4:>5:>1:>2:>Nil,4:>5:>1:>2:>3:>Nil):>Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
