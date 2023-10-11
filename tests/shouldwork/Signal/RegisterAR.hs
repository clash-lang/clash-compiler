{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module RegisterAR where

-- Register: Asynchronous, Regular

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

testInput :: Vec 7 (Signed 8)
testInput = 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> Nil

resetInput
  :: Clock dom
  -> Reset dom
  -> Enable dom
  -> Signal dom Bool
resetInput clk reset en
  = register clk reset en True
  $ register clk reset en False
  $ register clk reset en False
  $ register clk reset en True
  $ register clk reset en True
  $ pure False

topEntity
  :: Clock System
  -> Reset System
  -> Signal System (Signed 8)
topEntity clk rst = head <$> r
  where
    r = register clk rst enableGen testInput (flip rotateLeftS d1 <$> r)

topEntityAR clk rst = topEntity clk arst
  where
    arst = unsafeFromActiveHigh (resetInput clk rst enableGen)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntityAR #-}

-- | Doing this case inline trips GHC 8.4 due to dead code. We sometimes
-- want to run our whole testsuite with a different System domain though, so
-- it's not _really_ dead code.
oneOrThree :: SResetKind dom -> (Signed 8)
oneOrThree = \case {SAsynchronous -> 1; SSynchronous -> 3}

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier' clk rst (1 :> 1 :> 2 :> oneOrThree (resetKind @System) :> 1 :> 1 :> 2 :> 3 :> Nil)
    done           = expectedOutput (topEntityAR clk rst)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
