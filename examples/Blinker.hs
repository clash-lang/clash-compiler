module Blinker where

import CLaSH.Prelude
import CLaSH.Promoted.Symbol
import CLaSH.Intel.ClockGen

type Dom50 = Dom "System" 20000

{-# ANN topEntity
  (defTop
    { t_name   = "blinker"
    , t_inputs = [ PortName "CLOCK_50"
                 , PortName "KEY0"
                 , PortName "KEY1"
                 ]
    , t_output = PortName "LED"
    }) #-}
topEntity :: Clock Dom50 Source -> Reset Dom50 Asynchronous -> Signal Dom50 Bit -> Signal Dom50 (BitVector 8)
topEntity clk rst key1 =
    let  (pllOut,pllStable) = altpll (SSymbol @ "altpll50") clk rst
         rstSync            = resetSynchronizer pllOut (unsafeToAsyncReset pllStable)
    in   withClockReset pllOut rstSync leds
  where
    key1R  = isRising 1 key1
    leds   = mealy blinkerT (1,False,0) key1R

blinkerT (leds,mode,cntr) key1R = ((leds',mode',cntr'),leds)
  where
    -- clock frequency = 50e6  (50 MHz)
    -- led update rate = 333e-3 (every 333ms)
    cnt_max = 16650000 -- 50e6 * 333e-3

    cntr' | cntr == cnt_max = 0
          | otherwise       = cntr + 1

    mode' | key1R     = not mode
          | otherwise = mode

    leds' | cntr == 0 = if mode then complement leds
                                else rotateL leds 1
          | otherwise = leds
