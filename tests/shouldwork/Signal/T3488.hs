module T3488 where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import Test.Tasty.Clash.CollectSimResults

import T3488Definitions

topEntity ::
  Clock System ->
  Signal System (RamOp 4 Payload) ->
  Signal System (RamOp 4 Payload) ->
  (Signal System Payload, Signal System Payload)
topEntity = tdpRam
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  expected = $(collectSimResults (length ops + 1) simOut)
  (opsA, opsB) = unbundle (stimuliGenerator clk noReset ops)
  done =
    outputVerifier' clk noReset expected (observe clk (topEntity clk opsA opsB))
  clk = tbSystemClockGen (not <$> done)
{-# OPAQUE testBench #-}
