module ResetGen where

import Clash.Explicit.Prelude

topEntity
  :: Clock System
  -> Signal System (Bool, Bool)
topEntity clk = bundle (unsafeToHighPolarity r, unsafeToHighPolarity r')
  where
    r  = resetGenN (SNat @3)
    r' = holdReset clk enableGen (SNat @2) r
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput =
      outputVerifier
        clk
        rst
        -- Note that outputVerifier skips first sample
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
