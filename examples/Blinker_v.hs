module Blinker_v where

import CLaSH.Prelude

{-# ANN topEntity
  (defTop
    { t_name     = "blinker"
    , t_inputs   = ["KEY1"]
    , t_outputs  = ["LED"]
    , t_extraIn  = [ ("CLOCK_50", 1)
                   , ("KEY0"    , 1)
                   ]
    , t_clocks   = [ altpll "altpll50" "CLOCK_50[0]" "~ KEY0[0]" ]
    }) #-}
topEntity :: Signal Bit -> Signal (BitVector 8)
topEntity key1 = leds
  where
    key1R = isRising 1 key1
    leds  = mealy blinkerT (1,False,0) key1R

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
