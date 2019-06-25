module HOClock where

import Clash.Prelude

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Vec 8 (Signal System (Int,Int))
  -> Vec 8 (Signal System (Int,Int))
topEntity = exposeClockResetEnable (map (register (0,0)))
