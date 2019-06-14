module BlockRamTest where

import Clash.Prelude

topEntity
  :: Clock  System
  -> Enable System
  -> Signal System (Unsigned 7)
  -> Signal System (Maybe (Unsigned 7,Unsigned 4))
  -> Signal System (Unsigned 4)
topEntity clk en = exposeEnable (exposeClock (blockRamPow2 (repeat 0)) clk) en
