module NestedPrimitives where

import Clash.Prelude

topEntity :: Vec 3 (Signed 16)
topEntity = map resize o
  where
    o :: Vec 3 (Signed 32)
    o = replicate d3 (bit 31)
