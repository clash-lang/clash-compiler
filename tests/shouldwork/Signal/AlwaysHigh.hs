module AlwaysHigh where

import Clash.Prelude

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System Bit
topEntity = exposeClockResetEnable (register high (pure high))
