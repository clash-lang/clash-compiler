module VIndicesI where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: Signal System (Vec 4 (SatIndex 'SatError 4))
topEntity = pure indicesI
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier' clk rst ((0:>1:>2:>3:>Nil):>Nil)
    done           = expectedOutput topEntity
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
