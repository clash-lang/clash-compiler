{-# LANGUAGE CPP #-}

module BlobVec where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

topEntity
  :: Clock System
  -> Signal System (Unsigned 4)
  -> Signal System (Vec 3 (BitVector 8))

topEntity clk addr = bundle $ romBlob clk enableGen <$> blobs <*> pure addr
 where
  blobs =    $(memBlobTH Nothing [ 1 :: BitVector 8 .. 15])
          :> $(memBlobTH Nothing [17 :: BitVector 8 .. 31])
          :> $(memBlobTH Nothing [33 :: BitVector 8 .. 47])
          :> Nil
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

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
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBench #-}
