module NestedPrimitives2 where

import CLaSH.Prelude

topEntity :: Vec 5 (Vec 3 (Signed 32))
topEntity = vmap (vtake d3) o
  where
    o :: Vec 5 (Vec 5 (Signed 32))
    o = vcopy d5 (vcopy d5 (bit 31))
