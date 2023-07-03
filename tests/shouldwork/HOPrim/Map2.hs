{-# LANGUAGE CPP #-}

module Map2 where

import Clash.Prelude
import Clash.Explicit.Testbench

go
  :: Signal System ()
  -> Signal System Bool
go _ = pure True

topEntity
  :: Signal System (Vec 4 ())
  -> Signal System (Vec 4 Bool)
topEntity =
  bundle . fmap go . unbundle
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}


testBench :: SystemClockResetEnable => Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst ((repeat ()) :> Nil)
    expectedOutput = outputVerifier' clk rst ((repeat True):>Nil)
    done           = expectedOutput (topEntity testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
