{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- Test for https://github.com/clash-lang/clash-compiler/issues/601
module SignedProjectionTB where
import Clash.Prelude
import Clash.Explicit.Testbench
import SignedProjection
import qualified Data.List as L


expected :: Vec _ (Signed 4)
expected = $(listToVecTH $ L.take (length input) $ simulate minimal $ toList input)

topEntity
  :: Clock System 'Source
  -> Reset System 'Asynchronous
  -> Signal System (Complex (Signed 3))
  -> Signal System (Signed 4)
topEntity clk rst = withClockReset clk rst top
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst input
    expectedOutput = outputVerifier clk rst expected
    done           = expectedOutput (topEntity clk rst testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
