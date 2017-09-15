module BlockRamTest where

import Clash.Prelude

topEntity
  :: SystemClockReset
  => Signal System (Unsigned 7)
  -> Signal System (Maybe (Unsigned 7,Unsigned 4))
  -> Signal System (Unsigned 4)
topEntity = blockRamPow2 (repeat 0)
