module ClassOps where

import CLaSH.Prelude

topEntity :: (Integer,Integer) -> Integer
topEntity = uncurry mod

testInput :: Signal (Integer,Integer)
testInput = stimuliGenerator $(v [(19,4)::(Integer,Integer),(7,3),(55,-10),(9,-2),(11,10)])

expectedOutput :: Signal Integer -> Signal Bool
expectedOutput = outputVerifier $(v ([3::Integer,1,-5,-1,1]))
