module VIndex where

import Clash.Prelude

topEntity :: (Integer,Vec 8 (Vec 8 (Maybe Int))) -> Vec 8 (Maybe Int)
topEntity (i,as) = zipWith (!!) as (iterateI (+1) i)
