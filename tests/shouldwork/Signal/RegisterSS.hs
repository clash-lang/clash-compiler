module RegisterSS where

import Clash.Explicit.Prelude

testInput :: Vec 7 (Signed 8)
testInput = 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> Nil

resetInput
  :: Clock domain gated
  -> Reset domain sync
  -> Signal domain Bool
resetInput clk reset
  = register clk reset True
  $ register clk reset False
  $ register clk reset False
  $ register clk reset True
  $ register clk reset True
  $ pure False

topEntity
  :: Clock System Source
  -> Reset System Synchronous
  -> Signal System (Signed 8)
topEntity clk rst = head <$> r
  where
    r = register clk rst testInput (flip rotateLeftS d1 <$> r)

topEntitySS clk rst = topEntity clk srst
  where
    srst = unsafeToSyncReset (resetInput clk rst)
{-# NOINLINE topEntitySS #-}

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier clk rst (1 :> 1 :> 2 :> 3 :> 1 :> 1 :> 2 :> 3 :> Nil)
    done           = expectedOutput (topEntitySS clk rst)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
