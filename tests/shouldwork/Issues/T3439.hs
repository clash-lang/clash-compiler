module T3439 where

import Clash.Prelude

data T = T deriving (Enum)

topEntity :: Int -> T
topEntity = toEnum
