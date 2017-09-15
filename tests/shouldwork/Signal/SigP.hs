module SigP where

import Clash.Prelude

topEntity
  :: SystemClockReset
  => Signal System (Bool, Bool)
  -> (Signal System Bool, Signal System Bool)
topEntity = unbundle . register (False,False)
