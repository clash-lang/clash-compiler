module AlwaysHigh where

import Clash.Prelude

topEntity
  :: SystemClockReset
  => Signal System Bit
topEntity = register high (pure high)
