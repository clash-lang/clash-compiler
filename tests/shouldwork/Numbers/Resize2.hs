module Resize2 where

import CLaSH.Prelude

topEntity :: Signed 4 -> Signed 5
topEntity = resize

testInput :: Signal (Signed 4)
testInput = stimuliGenerator $(v ([minBound .. maxBound]::[Signed 4]))

expectedOutput :: Signal (Signed 5) -> Signal Bool
expectedOutput = outputVerifier $(v ([-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7]::[Signed 5]))
