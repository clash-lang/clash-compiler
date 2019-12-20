{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module PopCount where

import Clash.Prelude
import Clash.Explicit.Testbench
import GHC.Word
import GHC.Int
import Data.Bits

topEntity :: Integer -> Vec _ Int
topEntity i =  popCount @Word j
            :> popCount @Word8 j
            :> popCount @Word16 j
            :> popCount @Word32 j
            :> popCount @Word64 j
            :> popCount @Int j
            :> popCount @Int8 j
            :> popCount @Int16 j
            :> popCount @Int32 j
            :> popCount @Int64 j
            :> popCount @(BitVector 7) j
            :> popCount @(Signed 9) j
            :> popCount @(Signed 101) j
            :> popCount @(Unsigned 11) j
            :> Nil
  where
    j :: Num a => a
    j = fromInteger i
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst $(listToVecTH [1::Integer,3,8,50,0])
    expectedOutput = outputVerifier'   clk rst $ fmap repeat $(listToVecTH ([1,2,1,3,0]::[Int]))
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
