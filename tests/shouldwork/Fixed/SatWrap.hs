module SatWrap where

import Clash.Prelude

topEntity:: (SFixed 2 6) -> (SFixed 2 6) -> (SFixed 2 6)
topEntity = satAdd SatWrap
