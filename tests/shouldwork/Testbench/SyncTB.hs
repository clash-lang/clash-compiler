module SyncTB where

import Clash.Explicit.Prelude
import Clash.Signal (mux)

type Dom2 = Dom "dom" 2
type Dom7 = Dom "dom" 7
type Dom9 = Dom "dom" 9

topEntity
  :: Clock Dom2 Source
  -> Clock Dom7 Source
  -> Clock Dom9 Source
  -> Signal Dom7 Integer
  -> Signal Dom9 Integer
topEntity clk2 clk7 clk9 i =
  delay
    clk9
    0
    (unsafeSynchronizer
      clk2
      clk9
      (delay
        clk2
        0
        (unsafeSynchronizer
          clk7
          clk2
          (delay clk7 0 i))))
{-# NOINLINE topEntity #-}

testBench
  :: Signal Dom9 Bool
testBench = done
  where
    testInput      = stimuliGenerator clk7 rst7 $(listToVecTH [(1::Integer)..20])
    expectedOutput = outputVerifier   clk9 rst9
                        (0 :> 1 :>  $(listToVecTH ([2,3,4,6,7,8,9,11,12,13,15,16]::[Integer])))
    done           = expectedOutput (topEntity clk2 clk7 clk9 testInput)
    done'          = not <$> done
    clk2           = tbClockGen @Dom2 (unsafeSynchronizer clk9 clk2 done')
    clk7           = tbClockGen @Dom7 (unsafeSynchronizer clk9 clk7 done')
    clk9           = tbClockGen @Dom9 done'
    rst7           = asyncResetGen @Dom7
    rst9           = asyncResetGen @Dom9
