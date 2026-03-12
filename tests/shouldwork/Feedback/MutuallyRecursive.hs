{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module MutuallyRecursive where

import qualified Clash.Explicit.Prelude as Explicit
import Clash.Explicit.Testbench (outputVerifier', tbClockGen)
import Clash.Prelude
import Clash.Prelude.Testbench (stimuliGenerator)

hdlSimTest ::
  forall a b n.
  (1 <= n) =>
  -- | Reset for /n/ cycles
  SNat n ->
  -- | Top entity
  ((SystemClockResetEnable) => a -> Signal System b) ->
  -- | Test input
  ((SystemClockResetEnable) => a) ->
  -- | Expected output
  ((SystemClockResetEnable) => Signal System b -> Signal System Bool) ->
  Signal System Bool
hdlSimTest n topEntity testInput expectedOutput = done
 where
  tbClk = tbClockGen (not <$> done)
  tbRst = resetGenN @System n
  tbEn = enableGen

  expectedOutput' = withClockResetEnable tbClk tbRst tbEn expectedOutput
  topEntity' = withClockResetEnable tbClk tbRst tbEn topEntity
  testInput' = withClockResetEnable tbClk tbRst tbEn testInput

  done = expectedOutput' (topEntity' testInput')
{-# OPAQUE hdlSimTest #-}

testBench :: Signal System Bool
testBench = done
 where
  done = hdlSimTest d1 topEntity testInput expectedOutput

topEntity ::
  (SystemClockResetEnable) =>
  Signal System (BitVector 8) ->
  Signal System (BitVector 8)
topEntity =
  Explicit.register hasClock Explicit.noReset enableGen 0

testInput ::
  (SystemClockResetEnable) =>
  Signal System (BitVector 8)
testInput = stimuliGenerator (0 :> 1 :> 2 :> Nil)

expectedOutput ::
  (SystemClockResetEnable) =>
  Signal System (BitVector 8) ->
  Signal System Bool
expectedOutput =
  outputVerifier' hasClock hasReset (0 :> 0 :> 1 :> Nil)
