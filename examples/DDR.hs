module DDR where

import CLaSH.Explicit.Prelude

type DomA1 = Dom "A" 1 -- fast, twice as fast as slow
type DomB2 = Dom "B" 2 -- slow

-- | Given a "fast" signal:
--
-- >>> let x = fromList [1,2,3,4,5,6,7,8] :: Signal' Clk1 (Unsigned 8)
-- >>> sampleN 5 (topEntity x)
-- [(0,0),(1,2),(3,4),(5,6),(7,8)]
--
-- The output is double in width, at half the speed
topEntity
  :: Clock DomA1 Source
  -> Reset DomA1 Asynchronous
  -> Clock DomB2 Source
  -> Signal DomA1 (Unsigned 8)
  -> Signal DomB2 (Unsigned 8, Unsigned 8)
topEntity clk1 rst1 clk2 i =
  let h = register clk1 rst1 0 (register clk1 rst1 0 i)
      l = register clk1 rst1 0 i
  in  unsafeSynchronizer clk1 clk2 (bundle (h,l))
{-# ANN topEntity (TestBench 'tb) #-}

tb
  :: Signal DomB2 Bool
tb = done
  where
    testInput      = stimuliGenerator clkA1 rstA1 $(listToVecTH [1::Unsigned 8,2,3,4,5,6,7,8])
    expectedOutput = outputVerifier   clkB2 rstB2 $(listToVecTH [(0,0) :: (Unsigned 8, Unsigned 8),(1,2),(3,4),(5,6),(7,8)])
    done           = expectedOutput (topEntity clkA1 rstA1 clkB2 testInput)
    done'          = not <$> done
    clkA1          = tbClockGen @DomA1 (unsafeSynchronizer clkB2 clkA1 done')
    clkB2          = tbClockGen @DomB2 done'
    rstA1          = asyncResetGen @DomA1
    rstB2          = asyncResetGen @DomB2
