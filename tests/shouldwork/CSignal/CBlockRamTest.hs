module CBlockRamTest where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit

type Clk10 = Clk "clk" 10

clk10 :: SClock Clk10
clk10 = sclock


topEntity :: CSignal Clk10 (Unsigned 7)
          -> CSignal Clk10 (Unsigned 7)
          -> CSignal Clk10 (Bool)
          -> CSignal Clk10 (Vec 4 Bit)
          -> CSignal Clk10 (Vec 4 Bit)
topEntity = cblockRam clk10 (replicate d128 (repeat high))
