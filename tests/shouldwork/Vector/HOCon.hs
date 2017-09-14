module HOCon where

import Clash.Prelude

topEntity :: Vec 8 Int -> Vec 8 (Maybe Int)
topEntity = map Just
