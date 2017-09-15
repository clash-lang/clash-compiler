module HOPrim where

import Clash.Prelude

topEntity :: Vec 4 Integer -> Vec 4 (Unsigned 4)
topEntity = map fromInteger
