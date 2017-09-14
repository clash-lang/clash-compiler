module Split where

import Clash.Prelude

topEntity :: Vec 7 Bit -> Vec 2 Bit
topEntity vs = snd (splitAtI vs)
