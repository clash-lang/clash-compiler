module HOPrim where

import CLaSH.Prelude

topEntity :: Vec 4 Integer -> Vec 4 (Unsigned 4)
topEntity = vmap fromInteger
