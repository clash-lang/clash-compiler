module Blinker where

import Clash.Prelude
import Clash.Intel.ClockGen

type DomInput = Dom "Input" 20000
type Dom50 = Dom "System" 20000

{-# ANN topEntity
  (Synthesize
    { t_name   = "blinker"
    , t_inputs = [ PortName "CLOCK_50"
                 , PortName "KEY0"
                 , PortName "KEY1"
                 ]
    , t_output = PortName "LED"
    }) #-}
topEntity
  :: Clock DomInput Source
  -> Signal DomInput Bool
  -> Signal Dom50 Bit
  -> Signal Dom50 (BitVector 8)
topEntity clk rst =
    exposeClockReset (mealy blinkerT (1,False,0) . isRising 1) pllOut rstSync
  where
    (pllOut,pllStable) = altpll @Dom50 (SSymbol @"altpll50") clk (unsafeToAsyncReset (not <$> rst))
    rstSync            = resetSynchronizer pllOut (unsafeToAsyncReset (not <$> pllStable))

blinkerT (leds,mode,cntr) key1R = ((leds',mode',cntr'),leds)
  where
    -- clock frequency = 50e6  (50 MHz)
    -- led update rate = 333e-3 (every 333ms)
    cnt_max = 16650000 :: (Index 16650001) -- 50e6 * 333e-3

    cntr' | cntr == cnt_max = 0
          | otherwise       = cntr + 1

    mode' | key1R     = not mode
          | otherwise = mode

    leds' | cntr == 0 = if mode then complement leds
                                else rotateL leds 1
          | otherwise = leds
