module SyncTB where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit

clk2 = Clock d2
clk3 = Clock d3
clk4 = Clock d4
clk5 = Clock d5
clk6 = Clock d6
clk7 = Clock d7
clk9 = Clock d9

topEntity :: CSignal 7 Int -> CSignal 9 Int
topEntity i = cregister (Clock d9) 70 (veryUnsafeSynchronizer (Clock d2) (Clock d9) (cregister (Clock d2) 99 (veryUnsafeSynchronizer (Clock d7) (Clock d2) (cregister (Clock d7) 50 i))))

testInput :: CSignal 7 Int
testInput = cstimuliGenerator $(v [(1::Int)..10]) clk7

expectedOutput :: CSignal 9 Int -> CSignal 9 Bool
expectedOutput = coutputVerifier $(v ([70,99,2,3,4,5,7,8,9,10]::[Int])) clk9
