module HOClock where

import Clash.Prelude

topEntity
  :: SystemClockReset
  => Vec 8 (Signal System (Int,Int)) -> Vec 8 (Signal System (Int,Int))
topEntity = map (register (0,0))
