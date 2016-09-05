module VReplace where

import CLaSH.Prelude

topEntity :: (Integer,Unsigned 4,Vec 8 (Unsigned 4)) -> Vec 8 (Vec 8 (Unsigned 4))
topEntity (i,j,as) = zipWith (\i u -> replace i u as) (iterateI (+1) i) ((iterateI (subtract 1) j))

testInput :: Signal (Integer,Unsigned 4,Vec 8 (Unsigned 4))
testInput = stimuliGenerator $(listToVecTH ([ (0,8,replicate d8 0)
                                  ]::[(Integer,Unsigned 4,Vec 8 (Unsigned 4))]))
