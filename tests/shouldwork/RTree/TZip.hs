module TZip where

import CLaSH.Prelude

topEntity :: RTree 3 Int -> RTree 3 Bool -> RTree 3 (Int,Bool)
topEntity = tzip

