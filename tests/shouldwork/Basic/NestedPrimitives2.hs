module NestedPrimitives2 where

import Clash.Prelude

topEntity :: Vec 5 (Vec 3 (Signed 32))
topEntity = map (take d3) o
  where
    o :: Vec 5 (Vec 5 (Signed 32))
    o = replicate d5 (replicate d5 (bit 31))
