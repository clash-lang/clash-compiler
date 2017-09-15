module TZip where

import Clash.Prelude

topEntity :: RTree 3 Int -> RTree 3 Bool -> RTree 3 (Int,Bool)
topEntity = tzip

