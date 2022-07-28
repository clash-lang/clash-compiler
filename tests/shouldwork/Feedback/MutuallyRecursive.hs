{-# LANGUAGE RankNTypes #-}

module MutuallyRecursive where

import           Clash.Explicit.Testbench (biTbClockGen, outputVerifier')
import           Clash.Prelude.Testbench  (stimuliGenerator)
import qualified Clash.Explicit.Prelude   as Explicit
import           Clash.Prelude
import           Data.Proxy               (Proxy (..))

hdlSimTest
  :: forall dom a b n
   . (1 <= n, KnownDomain dom)
  => SNat n
  -- ^ Reset for /n/ cycles
  -> (HiddenClockResetEnable dom => a -> Signal dom b)
  -- ^ Top entity
  -> (HiddenClockResetEnable dom => Proxy dom -> a)
  -- ^ Test input
  -> ((HiddenClockResetEnable dom, SystemClockResetEnable) => Signal dom b -> Signal System Bool)
  -- ^ Expected output
  -> Signal System Bool
hdlSimTest n topEntity testInput expectedOutput = done
  where
    (tbClk, dutClk) = biTbClockGen (not <$> done)
    (tbRst, dutRst) = (resetGenN @System n, resetGenN @dom n)
    (tbEn, dutEn)   = (enableGen, enableGen)

    topEntity' = withSpecificClockResetEnable dutClk dutRst dutEn topEntity
    testInput' = withSpecificClockResetEnable dutClk dutRst dutEn testInput Proxy

    expectedOutput' =
      withSpecificClockResetEnable dutClk dutRst dutEn $
        withSpecificClockResetEnable tbClk tbRst tbEn expectedOutput

    done = expectedOutput' (topEntity' testInput')
{-# NOINLINE hdlSimTest #-}

testBench :: Signal System Bool
testBench = done
 where
  (_, clk) = biTbClockGen (not <$> done)
  done     = hdlSimTest d1 (topEntity clk) testInput expectedOutput

topEntity
  :: Clock System
  -> Signal System (BitVector 8)
  -> Signal System (BitVector 8)
topEntity clk =
  Explicit.register clk resetGen enableGen 0

testInput
  :: SystemClockResetEnable
  => Proxy System
  -> Signal System (BitVector 8)
testInput _proxy = stimuliGenerator (0 :> 1 :> 2 :> Nil)

expectedOutput
  :: SystemClockResetEnable
  => Signal System (BitVector 8)
  -> Signal System Bool
expectedOutput =
  outputVerifier' hasClock hasReset (0 :> 0 :> 1 :> Nil)
