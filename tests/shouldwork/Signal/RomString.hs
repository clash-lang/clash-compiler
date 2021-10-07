module RomString where

import Clash.Prelude
import qualified Clash.Explicit.Prelude as E
import Clash.Explicit.ROM.File (createMemString, romString)
import Clash.Explicit.Testbench

createMemString "memData" Nothing [0 :: Unsigned 8 .. 255]

topEntity
  :: Clock System
  -> Enable System
  -> Signal System (Unsigned 8)
  -> Signal System (Unsigned 8)
topEntity clk en = fmap unpack . romString clk en memData
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput = E.register clk rst en 0 (testInput + 1)
    expectedOutput =
      outputVerifier' clk rst $(listToVecTH [0::Unsigned 8,0,1,2,3,4,5,6,7,8])
    done =
      expectedOutput (ignoreFor clk rst en d1 0 $ topEntity clk en testInput)
    clk = tbSystemClockGen (not <$> done)
    rst = systemResetGen
    en = enableGen
