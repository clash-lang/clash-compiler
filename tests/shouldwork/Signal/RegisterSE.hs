{-# LANGUAGE CPP #-}

module RegisterSE where

-- Register: Synchronous, Enabled

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

testInput :: Vec 7 (Signed 8)
testInput = 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> Nil

resetInput
  :: Clock dom
  -> Reset dom
  -> Enable dom
  -> Signal dom Bool
resetInput clk reset en
  = register clk reset en True
  $ register clk reset en False
  $ register clk reset en False
  $ register clk reset en True
  $ pure False

enableInput
  :: Clock dom
  -> Reset dom
  -> Enable dom
  -> Signal dom Bool
enableInput clk reset en
  = register clk reset en True
  $ register clk reset en True
  $ register clk reset en True
  $ register clk reset en True
  $ register clk reset en True
  $ register clk reset en False
  $ register clk reset en False
  $ register clk reset en True
  $ pure False

topEntity
  :: Clock XilinxSystem
  -> Reset XilinxSystem
  -> Enable XilinxSystem
  -> Signal XilinxSystem (Signed 8)
topEntity clk rst en = head <$> r
  where
    r = register clk rst en testInput (flip rotateLeftS d1 <$> r)

topEntitySE clk rst = topEntity clk arst en
  where
    arst = unsafeFromActiveHigh (resetInput clk rst enableGen)
    en = toEnable (enableInput clk rst enableGen)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntitySE #-}

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput        = outputVerifier testClk circuitClk resetGen (1 :> 1 :> 2 :> 3 :> 1 :> 2 :> 2 :> 2 :> 3 :> 3 :> 3 :> Nil)
    done                  = expectedOutput (topEntitySE circuitClk resetGen)
    (testClk, circuitClk) = biTbClockGen @System @XilinxSystem (not <$> done)
