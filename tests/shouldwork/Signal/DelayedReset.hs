{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
module DelayedReset where

import Clash.Explicit.Prelude
import Clash.Annotations.TopEntity
import Clash.Explicit.Testbench

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> (Signal System (Unsigned 4), Signal System (Unsigned 4), Signal System Bool)
topEntity clk reset en = (cycleCount, countFromReset, newResetSig)
  where
    newReset ::Reset System
    newReset
      = unsafeFromHighPolarity newResetSig

    newResetSig
      = register clk reset en True
      $ register clk reset en True
      $ pure False

    countFromReset :: Signal System (Unsigned 4)
    countFromReset' = countFromReset + 1
    countFromReset = register clk newReset en 0 countFromReset'

    cycleCount :: Signal System (Unsigned 4)
    cycleCount' = cycleCount + 1
    cycleCount = register clk reset en 0 cycleCount'
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier' clk rst
                       ((0,0,True) :>
                        (1,0,True) :>
                        (2,0,False) :>
                        (3,1,False) :> Nil)
    done           = expectedOutput (bundle (topEntity clk rst enableGen))
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
