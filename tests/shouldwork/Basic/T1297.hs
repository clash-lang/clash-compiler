module T1297 where

import Clash.Prelude
import Numeric.Natural (Natural)

topEntity :: Int
topEntity =
  case compare (toInteger @Natural (0 - 1)) 0 of
    LT -> 2
    _ -> 3
