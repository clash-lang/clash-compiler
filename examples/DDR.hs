module DDR where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit

type Clk1 = Clk "A" 1 -- fast, twice as fast as slow
type Clk2 = Clk "B" 2 -- slow

clk1 :: SClock Clk1
clk1 = sclock
clk2 :: SClock Clk2
clk2 = sclock

-- | Given a "fast" signal:
--
-- >>> let x = fromList [1,2,3,4,5,6,7,8] :: Signal' Clk1 (Unsigned 8)
-- >>> sampleN 5 (topEntity x)
-- [(0,0),(1,2),(3,4),(5,6),(7,8)]
--
-- The output is double in width, at half the speed
topEntity :: Signal' Clk1 (Unsigned 8) -> Signal' Clk2 (Unsigned 8, Unsigned 8)
topEntity i =
  let h = register' clk1 0 (register' clk1 0 i)
      l = register' clk1 0 i
  in  unsafeSynchronizer clk1 clk2 (bundle (h,l))

testInput :: Signal' Clk1 (Unsigned 8)
testInput = stimuliGenerator' clk1 $(listToVecTH [1::Unsigned 8,2,3,4,5,6,7,8])

expectedOutput :: Signal' Clk2 (Unsigned 8, Unsigned 8) -> Signal' Clk2 Bool
expectedOutput = outputVerifier' clk2 $(listToVecTH [(0,0) :: (Unsigned 8, Unsigned 8),(1,2),(3,4),(5,6),(7,8)])

