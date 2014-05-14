module VIndex where

import CLaSH.Prelude

topEntity :: (Integer,Vec 8 (Vec 8 (Maybe Int))) -> Vec 8 (Maybe Int)
topEntity (i,as) = vzipWith (!) as (viterateI (+1) i)
