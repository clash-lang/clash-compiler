module HOClock where

import Clash.Prelude

topEntity
  :: Clock System Source
  -> Reset System Asynchronous
  -> Vec 8 (Signal System (Int,Int)) -> Vec 8 (Signal System (Int,Int))
topEntity = exposeClockReset (map (register (0,0)))
