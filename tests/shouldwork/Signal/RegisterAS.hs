module RegisterAS where

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
  -> Reset System Asynchronous
  -> Signal System (Signed 8)
topEntity clk rst = head <$> r
  where
    r = register clk rst testInput (flip rotateLeftS d1 <$> r)

topEntityAS clk rst = topEntity clk arst
  where
    arst = unsafeToAsyncReset (resetInput clk rst)
{-# NOINLINE topEntityAS #-}

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier clk rst (1 :> 1 :> 2 :> 1 :> 1 :> 1 :> 2 :> 3 :> Nil)
    done           = expectedOutput (topEntityAS clk rst)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
