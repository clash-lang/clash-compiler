{-# LANGUAGE CPP #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- Test for https://github.com/clash-lang/clash-compiler/issues/601
module SignedProjectionTB where
import Clash.Prelude
import Clash.Explicit.Testbench
import SignedProjection
import qualified Data.List as L


expected :: Vec _ (Signed 4)
expected = $(listToVecTH $ L.take (length input) $ simulate @System minimal $ toList input)

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Complex (Signed 3))
  -> Signal System (Signed 4)
topEntity clk rst en = withClockResetEnable clk rst en top
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst input
    expectedOutput = outputVerifier' clk rst expected
    done           = expectedOutput (topEntity clk rst enableGen testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
