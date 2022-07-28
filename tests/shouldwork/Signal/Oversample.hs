module Oversample where

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
  :: Clock Dom7
  -> Clock Dom2
  -> Signal Dom7 (BitVector 8)
  -> Signal Dom2 (BitVector 8)
topEntity clkA clkB = unsafeSynchronizer clkA clkB


testBench :: Signal Dom2 Bool
testBench = done
 where
  (clkB, clkA) = biTbClockGen (not <$> done)
  rstA = resetGen
  rstB = resetGen

  res = topEntity clkA clkB (testInput clkA rstA)
  done = expectedOutput clkB rstB res

testInput
  :: Clock Dom7
  -> Reset Dom7
  -> Signal Dom7 (BitVector 8)
testInput clkA rstA = unbundle . stimuliGenerator clkA rstA $
  1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> 8 :> 9 :> 10 :> Nil

expectedOutput
  :: Clock Dom2
  -> Reset Dom2
  -> Signal Dom2 (BitVector 8)
  -> Signal Dom2 Bool
expectedOutput clkB rstB = outputVerifier' clkB rstB
  (  replicate d3 1 ++ replicate d4 2
  ++ replicate d3 3 ++ replicate d4 4
  ++ replicate d3 5 ++ replicate d4 6
  ++ replicate d3 7 ++ replicate d4 8
  ++ replicate d3 9 ++ replicate d4 10
  )
