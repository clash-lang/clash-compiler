module BlockRamTest where

import Clash.Prelude

topEntity
  :: Clock  System Source
  -> Signal System (Unsigned 7)
  -> Signal System (Maybe (Unsigned 7,Unsigned 4))
  -> Signal System (Unsigned 4)
topEntity = exposeClock (blockRamPow2 (repeat 0))
