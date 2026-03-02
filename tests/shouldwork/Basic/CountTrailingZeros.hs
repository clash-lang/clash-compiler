module CountTrailingZeros where

import Clash.Explicit.Testbench
import Clash.Prelude
import Data.Bits
import GHC.Word

topEntity :: Word -> Int
topEntity = countTrailingZeros
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  testInput = stimuliGenerator clk rst $ (listToVecTH [1 :: Word, 3, 8, 50, 0])
  expectedVec :: Vec 5 Int
  expectedVec = case finiteBitSize (0 :: Word) of
    64 -> 0 :> 0 :> 3 :> 1 :> 64 :> Nil
    32 -> 0 :> 0 :> 3 :> 1 :> 32 :> Nil
    n -> error ("Unsupported word size: " <> show n)
  expectedOutput = outputVerifier' clk rst expectedVec
  done = expectedOutput (topEntity <$> testInput)
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen
