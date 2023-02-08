{-# OPTIONS_GHC "-Wno-orphans" #-}
{-# OPTIONS_GHC "-fno-full-laziness" #-}
module BlinkerClockWizard where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Xilinx.ClockGen

-- Define a synthesis domain with a clock with a period of 50000 /ps/.
-- i.e. 20 MHz
createDomain vXilinxSystem{vName="Dom20", vPeriod=50000}

{-# ANN topEntity
  (Synthesize
    { t_name   = "blinker"
    , t_inputs = [ PortName "CLK100MHZ"
                 , PortName "ck_rst"
                 ]
    , t_output = PortName "led"
    }) #-}
topEntity ::
  Clock XilinxSystem ->
  Signal XilinxSystem Bool ->
  Signal Dom20 Bit
topEntity clk100 rstBtn = led
 where
  (clk20, pllStable) =
    clockWizard (SSymbol @"clk_wiz_20") clk100 $ unsafeFromLowPolarity rstBtn
  rstSync = resetSynchronizer clk20 $ unsafeFromLowPolarity pllStable
  en = enableGen
  led = register clk20 rstSync en 1 $ liftA2 xor led toggle
  toggle = fmap (boolToBit . (== maxBound)) cnt
  cnt :: Signal Dom20 (Index 10_000_000)
  cnt = register clk20 rstSync en 0 $ satSucc SatWrap <$> cnt
{-# NOINLINE topEntity #-}

testBench ::
  (Signal XilinxSystem Bool, Signal Dom20 Bit)
testBench = (done, led)
 where
  cnt :: Signal XilinxSystem (Index 30)
  cnt = register clk rst en 0 $ satSucc SatBound <$> cnt
  rstBtn = (< 10) <$> cnt
  done = (== maxBound) <$> cnt
  led = topEntity clk rstBtn
  clk = tbClockGen (not <$> done)
  rst = resetGen
  en = enableGen
{-# NOINLINE testBench #-}
