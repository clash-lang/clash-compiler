{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module MutuallyRecursive where

import           Clash.Explicit.Testbench (tbClockGen, outputVerifier')
import           Clash.Prelude.Testbench  (stimuliGenerator)
import qualified Clash.Explicit.Prelude   as Explicit
import           Clash.Prelude

hdlSimTest
  :: forall a b n
   . (1 <= n)
  => SNat n
  -- ^ Reset for /n/ cycles
  -> (SystemClockResetEnable => a -> Signal System b)
  -- ^ Top entity
  -> (SystemClockResetEnable => a)
  -- ^ Test input
  -> (SystemClockResetEnable => Signal System b -> Signal System Bool)
  -- ^ Expected output
  -> Signal System Bool
hdlSimTest n topEntity testInput expectedOutput = done
  where
    tbClk = tbClockGen (not <$> done)
    tbRst = resetGenN @System n
    tbEn = enableGen

    expectedOutput' = withClockResetEnable tbClk tbRst tbEn expectedOutput
    topEntity' = withClockResetEnable tbClk tbRst tbEn topEntity
    testInput' = withClockResetEnable tbClk tbRst tbEn testInput

    done = expectedOutput' (topEntity' testInput')
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE hdlSimTest #-}

testBench :: Signal System Bool
testBench = done
 where
  done = hdlSimTest d1 topEntity testInput expectedOutput

topEntity
  :: SystemClockResetEnable
  => Signal System (BitVector 8)
  -> Signal System (BitVector 8)
topEntity =
  Explicit.register hasClock Explicit.noReset enableGen 0

testInput
  :: SystemClockResetEnable
  => Signal System (BitVector 8)
testInput = stimuliGenerator (0 :> 1 :> 2 :> Nil)

expectedOutput
  :: SystemClockResetEnable
  => Signal System (BitVector 8)
  -> Signal System Bool
expectedOutput =
  outputVerifier' hasClock hasReset (0 :> 0 :> 1 :> Nil)
