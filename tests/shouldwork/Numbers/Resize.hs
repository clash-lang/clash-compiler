module Resize where

import CLaSH.Prelude

topEntity :: Signed 4 -> Signed 3
topEntity = resize

testInput :: Signal (Signed 4)
testInput = stimuliGenerator $(listToVecTH ([minBound .. maxBound]::[Signed 4]))

expectedOutput :: Signal (Signed 3) -> Signal Bool
expectedOutput = outputVerifier $(listToVecTH ([-4,-3,-2,-1,-4,-3,-2,-1,0,1,2,3,0,1,2,3]::[Signed 3]))
