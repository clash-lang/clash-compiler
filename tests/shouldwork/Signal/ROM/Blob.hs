module Blob where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

createMemBlob "content" Nothing [1 :: Unsigned 8 .. 16]

topEntity
  :: Clock System
  -> Enable System
  -> Signal System (Unsigned 4)
  -> Signal System (Unsigned 8, Unsigned 8)
topEntity clk en rd =
  let rom0 en0 = unpack <$> romBlob clk en0 content rd
  in bundle (rom0 enableGen, rom0 en)
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput = register clk rst en 0 (testInput + 1)
    expectedOutput = outputVerifier' clk rst $ map (\n -> (n, n)) $
                       $(listToVecTH $ [0 :: Unsigned 8 .. 8])
    done = expectedOutput $ ignoreFor clk rst en d1 (0, 0) $
             topEntity clk en testInput
    clk = tbSystemClockGen (not <$> done)
    rst = systemResetGen
    en = enableGen
{-# NOINLINE testBench #-}
