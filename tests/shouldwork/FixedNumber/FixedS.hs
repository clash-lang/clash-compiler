module FixedS where

import Clash.Prelude

type SF = SFixed 4 20

topEntity :: SF -> SF
topEntity x = x + 4.578

testInput :: Signal SF
testInput = stimuliGenerator $ $(v ([1.0, 2.0, 4.0] :: [SFixed 4 20] ))

expectedOutput :: Signal SF -> Signal Bool
expectedOutput = outputVerifier $ $(v ([5.578, 6.578, 8.578] :: [SFixed 4 20]))
