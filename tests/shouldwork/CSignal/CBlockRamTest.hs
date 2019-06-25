module CBlockRamTest where

import Clash.Explicit.Prelude

createDomain vSystem{vTag="DomA10", vPeriod=10}

topEntity
  :: Clock  DomA10
  -> Enable DomA10
  -> Signal DomA10 (Unsigned 7)
  -> Signal DomA10 (Maybe (Unsigned 7,Vec 4 Bit))
  -> Signal DomA10 (Vec 4 Bit)
topEntity clk en =
  blockRam clk en (replicate d128 (repeat high))
