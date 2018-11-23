{-# LANGUAGE RankNTypes #-}
module DelayedReset where

import Clash.Explicit.Prelude
import Clash.Annotations.TopEntity

topEntity
  :: Clock System Source
  -> Reset System Asynchronous
  -> (Signal System (Unsigned 4), Signal System (Unsigned 4), Signal System Bool)
topEntity clk reset = (cycleCount, countFromReset, newResetSig)
  where
    newReset :: Reset System Asynchronous
    newReset
      = unsafeToAsyncReset newResetSig

    newResetSig
      = register clk reset True
      $ register clk reset True
      $ pure False

    countFromReset :: Signal System (Unsigned 4)
    countFromReset' = countFromReset + 1
    countFromReset = register clk newReset 0 countFromReset'

    cycleCount :: Signal System (Unsigned 4)
    cycleCount' = cycleCount + 1
    cycleCount = register clk reset 0 cycleCount'
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier clk rst
                       ((0,0,True) :>
                        (1,0,True) :>
                        (2,0,False) :>
                        (3,1,False) :> Nil)
    done           = expectedOutput (bundle (topEntity clk rst))
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
