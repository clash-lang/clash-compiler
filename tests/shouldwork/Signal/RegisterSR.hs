module RegisterSR where

-- Register: Synchronous, Regular

import Clash.Explicit.Prelude

testInput :: Vec 7 (Signed 8)
testInput = 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> Nil

resetInput
  :: KnownDomain dom conf
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> Signal dom Bool
resetInput clk reset en
  = register clk reset en True
  $ register clk reset en False
  $ register clk reset en False
  $ register clk reset en True
  $ register clk reset en True
  $ pure False

topEntity
  :: Clock XilinxSystem
  -> Reset XilinxSystem
  -> Signal XilinxSystem (Signed 8)
topEntity clk rst = head <$> r
  where
    r = register clk rst enableGen testInput (flip rotateLeftS d1 <$> r)

topEntitySR clk rst = topEntity clk srst
  where
    srst = unsafeFromHighPolarity (resetInput clk rst enableGen)
{-# NOINLINE topEntitySR #-}

testBench :: Signal XilinxSystem Bool
testBench = done
  where
    expectedOutput = outputVerifier clk rst (1 :> 1 :> 2 :> 3 :> 1 :> 1 :> 2 :> 3 :> Nil)
    done           = expectedOutput (topEntitySR clk rst)
    clk            = tbClockGen (not <$> done)
    rst            = resetGen
