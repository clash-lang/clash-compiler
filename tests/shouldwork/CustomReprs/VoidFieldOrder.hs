{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A custom bit representation on a constructor whose /first/ field has a
-- zero-width `HWType`. Those fields used to be dropped from the `Product`
-- before `convertToCustomRepr` zipped the field types up with `crFieldAnns`,
-- so every annotation after the dropped field ended up on the wrong field.
module VoidFieldOrder where

import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving
import Clash.Explicit.Testbench
import Clash.Prelude

data Box = Box () (BitVector 2) (BitVector 2)
  deriving (Generic, NFDataX)

{-# ANN module (
  DataReprAnn
    $(liftQ [t|Box|])
    8
    [ ConstrRepr 'Box 0b1111_0000 0b0101_0000
        [ 0b0000_0000, 0b0000_1100, 0b0000_0011 ] ] ) #-}

deriveBitPack [t|Box|]

topEntity ::
  Signal System (BitVector 2) ->
  Signal System (BitVector 2) ->
  Signal System Box
topEntity = liftA2 (Box ())
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput0 = stimuliGenerator clk rst $ 0b00
                                         :> 0b01
                                         :> 0b10
                                         :> 0b11
                                         :> Nil

    testInput1 = stimuliGenerator clk rst $ 0b11
                                         :> 0b10
                                         :> 0b01
                                         :> 0b00
                                         :> Nil

    expectedOutput = outputVerifierBitVector' clk rst $ 0b0101_00_11
                                                     :> 0b0101_01_10
                                                     :> 0b0101_10_01
                                                     :> 0b0101_11_00
                                                     :> Nil

    done = expectedOutput (pack <$> topEntity testInput0 testInput1)
    clk  = tbSystemClockGen (not <$> done)
    rst  = systemResetGen
