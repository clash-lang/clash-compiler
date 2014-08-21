module IrrefError where

import Prelude

topEntity :: Maybe Int -> Int
topEntity ~(Just x) = x
