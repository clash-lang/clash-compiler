module CBlockRamTest where

import Clash.Explicit.Prelude

type DomA10 = Dom "A" 10

topEntity :: Clock  DomA10 Source
          -> Signal DomA10 (Unsigned 7)
          -> Signal DomA10 (Maybe (Unsigned 7,Vec 4 Bit))
          -> Signal DomA10 (Vec 4 Bit)
topEntity clk = blockRam clk (replicate d128 (repeat high))
