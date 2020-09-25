module Compression where

import           Clash.Signal.Internal (mux)
import           Clash.Explicit.Prelude
import           Clash.Explicit.Testbench (biTbClockGen, outputVerifier)

createDomain vSystem{vName="Dom7", vPeriod=700}

clk7 :: Clock Dom7
clk7 = clockGen

en7 :: Enable Dom7
en7 = enableGen

createDomain vSystem{vName="Dom2", vPeriod=200}

clk2 :: Clock Dom2
clk2 = clockGen

en2 :: Enable Dom2
en2 = enableGen

topEntity
  :: Clock Dom2
  -> Clock Dom7
  -> Signal Dom2 (Unsigned 8)
  -> Signal Dom7 (Unsigned 8)
topEntity clkA clkB = unsafeSynchronizer clkA clkB


testBench :: Signal Dom7 Bool
testBench = done
 where
  (clkB, clkA) = biTbClockGen (not <$> done)
  rstA = resetGen
  rstB = resetGen

  res = topEntity clkA clkB (testInput clkA rstA)
  done = expectedOutput clkB rstB res

testInput
  :: Clock Dom2
  -> Reset Dom2
  -> Signal Dom2 (Unsigned 8)
testInput clkA rstA = unbundle . stimuliGenerator clkA rstA $
  1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> 8 :> 9 :> 10 :> Nil

expectedOutput
  :: Clock Dom7
  -> Reset Dom7
  -> Signal Dom7 (Unsigned 8)
  -> Signal Dom7 Bool
expectedOutput clkB rstB = outputVerifierR clkB rstB 1 $
  4 :> 7 :> 10 :> Nil


outputVerifierR
  :: forall l a testDom circuitDom
   . ( KnownNat l
     , KnownDomain testDom
     , KnownDomain circuitDom
     , Eq a
     , ShowX a )
  => Clock testDom
  -- ^ Clock to which the testbench is synchronized to (but not necessarily
  -- the circuit under test)
  -> Reset testDom
  -- ^ Reset line of testbench
  -> a
  -- ^ Value to compare with during reset
  -> Vec l a
  -- ^ Samples to compare with
  -> Signal circuitDom a
  -- ^ Signal to verify
  -> Signal testDom Bool
  -- ^ True if all samples are verified
outputVerifierR clk rst iR samples i0 =
    let t1    = snatToNum (clockPeriod @circuitDom)
        t2    = snatToNum (clockPeriod @testDom)
        i1    = veryUnsafeSynchronizer t1 t2 i0
        en    = toEnable (pure True)
        (s,o) = unbundle (genT iR <$> register clk rst en 0 s <*> unsafeFromReset rst)
        (e,f) = unbundle o
        f'    = register clk rst en False f
        -- Only assert while not finished
    in  mux f' f' $ assert clk rst "outputVerifier" i1 e f'
  where
    genT :: a -> Index l -> Bool -> (Index l,(a,Bool))
    genT rV s r = (s',(if r then rV else samples !! s,finished))
      where
        maxI = toEnum (length samples - 1)

        s' = if s < maxI
                then s + 1
                else s

        finished = s == maxI
