module T1240 where

import Clash.Prelude

topEntity
  :: SystemClockResetEnable
  => Signal System (Vec 1 Bool)
  -> Signal System (Vec 1 Bool)
topEntity = register @System $(lift (iterate d1 not True))
