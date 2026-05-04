module HOPrim where

import Clash.Prelude

topEntity :: Vec 4 Int -> Vec 4 Bool
topEntity = map odd
