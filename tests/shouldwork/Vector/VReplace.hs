module VReplace where

import CLaSH.Prelude

topEntity :: (Integer,Unsigned 4,Vec 8 (Unsigned 4)) -> Vec 8 (Vec 8 (Unsigned 4))
topEntity (i,j,as) = zipWith (replace as) (iterateI (+1) i) ((iterateI (subtract 1) j))
