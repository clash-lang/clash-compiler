module SigP where

import Clash.Prelude

topEntity
  :: Clock System Source
  -> Reset System Asynchronous
  -> Signal System (Bool, Bool)
  -> (Signal System Bool, Signal System Bool)
topEntity = exposeClockReset (unbundle . register (False,False))
