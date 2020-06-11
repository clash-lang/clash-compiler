module Example.Project where

import Clash.Prelude

plus :: Signed 8 -> Signed 8 -> Signed 8
plus a b = a + b

topEntity :: Signed 8 -> Signed 8 -> Signed 8
topEntity = plus
