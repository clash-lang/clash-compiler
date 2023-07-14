{-# LANGUAGE CPP #-}

module ResetGen where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

topEntity
  :: Clock System
  -> Signal System (Bool, Bool)
topEntity clk = bundle (unsafeToActiveHigh r, unsafeToActiveHigh r')
  where
    r  = resetGenN (SNat @3)
    r' = holdReset clk enableGen (SNat @2) r
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput =
      outputVerifier'
        clk
        rst
        -- Note that outputVerifier' skips first sample
        (  (True, True)
        :> (True, True)
        :> (False, True)
        :> (False, True)
        :> (False, False)
        :> (False, False)
        :> Nil )

    done = expectedOutput (topEntity clk)
    clk  = tbClockGen (not <$> done)
    rst  = resetGen
