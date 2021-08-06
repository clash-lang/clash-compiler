module T478 where

import Clash.Prelude

topEntity :: Vec 6 Bool -> Bool
topEntity = (==) (pure False)
