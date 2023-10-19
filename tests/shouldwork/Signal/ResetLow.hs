{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module ResetLow where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

createDomain vSystem{vName="SystemLow", vResetPolarity=ActiveLow}

testInput :: Vec 7 (Signed 8)
testInput = 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> Nil

resetInput
  :: Clock dom
  -> Reset dom
  -> Enable dom
  -> Signal dom Bool
resetInput clk reset en
  = register clk reset en False
  $ register clk reset en True
  $ register clk reset en True
  $ register clk reset en True
  $ register clk reset en False
  $ pure True

topEntity
  :: Clock SystemLow
  -> Reset SystemLow
  -> Enable SystemLow
  -> Signal SystemLow (Signed 8)
topEntity clk rst en = head <$> r
  where
    r = register clk rst en testInput (flip rotateLeftS d1 <$> r)

topEntity1 clk rst = topEntity clk arst enableGen
  where
    arst = unsafeToReset (resetInput clk rst enableGen)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity1 #-}

-- | Doing this case inline trips GHC 8.4 due to dead code. We sometimes
-- want to run our whole testsuite with a different System domain though, so
-- it's not _really_ dead code.
oneOrFour :: SResetKind dom -> (Signed 8)
oneOrFour = \case {SAsynchronous -> 1; SSynchronous -> 4}

testBench :: Signal SystemLow Bool
testBench = done
  where
    expectedOutput = outputVerifier' clk rst (1 :> 1 :> 2 :> 3
                                               :> oneOrFour (resetKind @SystemLow)
                                               :> 1 :> 2 :> 3 :> 4
                                               :> 5 :> 6 :> Nil)
    done           = expectedOutput (topEntity1 clk rst)
    clk            = tbClockGen (not <$> done)
    rst            = resetGen
