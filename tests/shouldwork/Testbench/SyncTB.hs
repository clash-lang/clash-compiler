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

topEntity :: Signal' Clk7 Integer -> Signal' Clk9 Integer
topEntity i = register' clk9 70 (unsafeSynchronizer clk2 clk9 (register' clk2 99 (unsafeSynchronizer clk7 clk2 (register' clk7 50 i))))

testInput :: Signal' Clk7 Integer
testInput = stimuliGenerator' clk7 $(v [(1::Integer)..10])

expectedOutput :: Signal' Clk9 Integer -> Signal' Clk9 Bool
expectedOutput = outputVerifier' clk9 $(v ([70,99,2,3,4,5,7,8,9,10]::[Integer]))
