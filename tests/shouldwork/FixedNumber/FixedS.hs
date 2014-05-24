module FixedS where

import CLaSH.Prelude

type SF = SFixed 4 20

co = $$(fLit 4.578) :: SF

topEntity :: SF -> SF
topEntity x = x + co

testInput :: Signal SF
testInput = stimuliGenerator $ $(v ([$$(fLit 1.0), $$(fLit 2.0), $$(fLit 4.0)] :: [SFixed 4 20] ))

expectedOutput :: Signal SF -> Signal Bool
expectedOutput = outputVerifier $ $(v ([$$(fLit 5.578), $$(fLit 6.578), $$(fLit 8.578)] :: [SFixed 4 20]))
