{-# LANGUAGE CPP #-}

module HoldResetSync where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

topEntity
  :: Clock System
  -> Signal System (Bool, Bool, Bool, Bool)
topEntity clk = bundle (rBool, r0, r1, r2)
  where
    r = resetGenN (SNat @3)
    rBool = unsafeToActiveHigh r
    r0 = unsafeToActiveHigh (holdReset clk enableGen (SNat @0) r)
    r1 = unsafeToActiveHigh (holdReset clk enableGen (SNat @1) r)
    r2 = unsafeToActiveHigh (holdReset clk enableGen (SNat @2) r)
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput =
      outputVerifier'
        clk
        rst
        -- Note that outputVerifier' skips first sample
        (  (True,  True,  True,  True)
        :> (True,  True,  True,  True)
        :> (False, False, True,  True)
        :> (False, False, False, True)
        :> (False, False, False, False)
        :> Nil )

    done = expectedOutput (topEntity clk)
    clk  = tbClockGen (not <$> done)
    rst  = resetGen
