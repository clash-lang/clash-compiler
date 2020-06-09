module T1360 where

import Clash.Prelude

data StatusA = InactiveA | ActiveA deriving (Generic,NFDataX)

topEntity :: HiddenClockResetEnable System =>
  Signal System (Vec 2 StatusA) -> Signal System (Index 2) -> Signal System (Vec 2 StatusA)
topEntity input idx = out
  where
    out = replace <$> idx <*> pure InactiveA <*> input
