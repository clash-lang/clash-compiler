module NestedPrimitives where

import CLaSH.Prelude

topEntity :: Vec 3 (Signed 16)
topEntity = vmap resize o
  where
    o :: Vec 3 (Signed 32)
    o = vcopy d3 (bit 31)
