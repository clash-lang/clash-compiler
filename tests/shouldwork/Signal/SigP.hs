module SigP where

import Clash.Prelude

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Bool, Bool)
  -> (Signal System Bool, Signal System Bool)
topEntity = exposeClockResetEnable (unbundle . register (False,False))
