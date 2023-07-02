{-# LANGUAGE CPP #-}

module Time where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System Int
  -> Signal System Int
topEntity clk rst en ps = register clk rst en 0 (ps + 1)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst (1 :> 2 :> 3 :> Nil)
    expectedOutput = outputVerifier' clk rst (0 :> 2 :> 3 :> Nil)
    done           = expectedOutput (topEntity clk rst enableGen testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
