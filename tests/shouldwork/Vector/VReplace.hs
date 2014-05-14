module VReplace where

import CLaSH.Prelude

topEntity :: (Integer,Unsigned 4,Vec 8 (Unsigned 4)) -> Vec 8 (Vec 8 (Unsigned 4))
topEntity (i,j,as) = vzipWith (vreplace as) (viterateI (+1) i) ((viterateI (subtract 1) j))
