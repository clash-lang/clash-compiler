module SyncTB where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit

type Clk2 = Clk "clk" 2
type Clk7 = Clk "clk" 7
type Clk9 = Clk "clk" 9

clk2 :: SClock Clk2
clk2 = sclock

clk7 :: SClock Clk7
clk7 = sclock

clk9 :: SClock Clk9
clk9 = sclock

topEntity :: CSignal Clk7 Int -> CSignal Clk9 Int
topEntity i = cregister clk9 70 (veryUnsafeSynchronizer clk2 clk9 (cregister clk2 99 (veryUnsafeSynchronizer clk7 clk2 (cregister clk7 50 i))))

testInput :: CSignal Clk7 Int
testInput = cstimuliGenerator clk7 $(v [(1::Int)..10])

expectedOutput :: CSignal Clk9 Int -> CSignal Clk9 Bool
expectedOutput = coutputVerifier clk9 $(v ([70,99,2,3,4,5,7,8,9,10]::[Int]))
