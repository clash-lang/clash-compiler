module SatWrap where

import CLaSH.Prelude

topEntity:: (SFixed 2 6) -> (SFixed 2 6) -> (SFixed 2 6)
topEntity = satPlus SatWrap
