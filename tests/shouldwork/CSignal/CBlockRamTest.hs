module CBlockRamTest where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit

type Clk10 = Clk "clk" 10

clk10 :: SClock Clk10
clk10 = sclock


topEntity :: Signal' Clk10 (Unsigned 7)
          -> Signal' Clk10 (Unsigned 7)
          -> Signal' Clk10 (Bool)
          -> Signal' Clk10 (Vec 4 Bit)
          -> Signal' Clk10 (Vec 4 Bit)
topEntity = blockRam' clk10 (replicate d128 (repeat high))
