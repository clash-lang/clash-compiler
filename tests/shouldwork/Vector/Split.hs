module Split where

import CLaSH.Prelude

topEntity :: Vec 7 Bit -> Vec 2 Bit
topEntity vs = snd (splitAtI vs)
