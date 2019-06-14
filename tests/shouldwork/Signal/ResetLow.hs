module ResetLow where

import Clash.Explicit.Prelude

createDomain vSystem{vTag="SystemLow", vPolarity=ActiveLow}

testInput :: Vec 7 (Signed 8)
testInput = 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> Nil

resetInput
  :: KnownDomain dom conf
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> Signal dom Bool
resetInput clk reset en
  = register clk reset en False
  $ register clk reset en True
  $ register clk reset en True
  $ register clk reset en True
  $ register clk reset en False
  $ pure True

topEntity
  :: Clock SystemLow
  -> Reset SystemLow
  -> Enable SystemLow
  -> Signal SystemLow (Signed 8)
topEntity clk rst en = head <$> r
  where
    r = register clk rst en testInput (flip rotateLeftS d1 <$> r)

topEntity1 clk rst = topEntity clk arst enableGen
  where
    arst = unsafeToReset (resetInput clk rst enableGen)
{-# NOINLINE topEntity1 #-}

testBench :: Signal SystemLow Bool
testBench = done
  where
    expectedOutput = outputVerifier clk rst (1 :> 1 :> 2 :> 3 :> 1 :> 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> Nil)
    done           = expectedOutput (topEntity1 clk rst)
    clk            = tbClockGen (not <$> done)
    rst            = resetGen
