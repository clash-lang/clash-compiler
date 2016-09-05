module SFixedTest where

import CLaSH.Prelude

type SF = SFixed 4 18

topEntity :: SF -> SF
topEntity x = x * 2.56

testInput :: Signal SF
testInput = stimuliGenerator $ $(listToVecTH ([1.2, 1.8, 3.5] :: [SFixed 4 18] ))

expectedOutput :: Signal SF -> Signal Bool
expectedOutput = outputVerifier $ $(listToVecTH ([3.07199, 4.607991, 8.96] :: [SFixed 4 18]))
