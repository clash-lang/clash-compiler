module AlwaysHigh where

import CLaSH.Prelude

topEntity
  :: SystemClockReset
  => Signal System Bit
topEntity = register high (pure high)
