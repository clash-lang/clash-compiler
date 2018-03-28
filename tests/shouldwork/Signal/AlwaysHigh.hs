module AlwaysHigh where

import Clash.Prelude

topEntity
  :: Clock System Source
  -> Reset System Asynchronous
  -> Signal System Bit
topEntity = exposeClockReset (register high (pure high))
