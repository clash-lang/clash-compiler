module BlobVec where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

topEntity
  :: Clock System
  -> Signal System (Unsigned 4)
  -> Signal System (Vec 3 (BitVector 8))

topEntity clk addr = bundle $ romBlob clk enableGen <$> blobs <*> pure addr
 where
  blobs =    $(memBlobTH @8 Nothing [ 1 .. 15])
          :> $(memBlobTH @8 Nothing [17 .. 31])
          :> $(memBlobTH @8 Nothing [33 .. 47])
          :> Nil
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput = register clk rst en 0 (testInput + 1)
    expectedOutput =
      outputVerifier' clk rst $ transpose
        (   $(listToVecTH $ [ 0 :: BitVector 8 ..  8])
         :> $(listToVecTH $ [16 :: BitVector 8 .. 24])
         :> $(listToVecTH $ [32 :: BitVector 8 .. 40])
         :> Nil
        )
    done =
      expectedOutput $ ignoreFor clk rst en d1 (0 :> 16 :> 32 :> Nil) $
        topEntity clk testInput
    clk = tbSystemClockGen (not <$> done)
    rst = systemResetGen
    en = enableGen
{-# NOINLINE testBench #-}
