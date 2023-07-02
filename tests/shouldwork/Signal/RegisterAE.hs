{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module RegisterAE where

-- Register: Asynchronous, Enabled

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

testInput :: Vec 7 (Signed 8)
testInput = 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> Nil

resetInput
  :: KnownDomain dom
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> Signal dom Bool
resetInput clk reset en
  = register clk reset en True
  $ register clk reset en False
  $ register clk reset en False
  $ register clk reset en True
  $ pure False

enableInput
  :: KnownDomain dom
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> Signal dom Bool
enableInput clk reset en
  = register clk reset en True
  $ register clk reset en True
  $ register clk reset en True
  $ register clk reset en True
  $ register clk reset en True
  $ register clk reset en False
  $ register clk reset en False
  $ register clk reset en True
  $ pure False

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Signed 8)
topEntity clk rst en = head <$> r
  where
    r = register clk rst en testInput (flip rotateLeftS d1 <$> r)

topEntityAE clk rst = topEntity clk arst en
  where
    arst = unsafeFromHighPolarity (resetInput clk rst enableGen)
    en = toEnable (enableInput clk rst enableGen)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntityAE #-}

-- | Doing this case inline trips GHC 8.4 due to dead code. We sometimes
-- want to run our whole testsuite with a different System domain though, so
-- it's not _really_ dead code.
oneOrThree :: SResetKind dom -> (Signed 8)
oneOrThree = \case {SAsynchronous -> 1; SSynchronous -> 3}

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier' clk rst (1 :> 1 :> 2 :> oneOrThree (resetKind @System) :>
                                             1 :> 2 :> 2 :> 2 :>
                                             3 :> 3 :> 3 :> Nil)
    done           = expectedOutput (topEntityAE clk rst)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
