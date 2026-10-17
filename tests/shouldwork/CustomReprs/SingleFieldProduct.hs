{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A custom bit representation on a type with a single constructor holding a
-- single field. 'mkADT' used to collapse such a type into the 'HWType' of its
-- field, leaving 'convertToCustomRepr' without a 'Product' to work with.
module SingleFieldProduct where

import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving
import Clash.Explicit.Testbench
import Clash.Prelude

data Box = Box (BitVector 2)
  deriving (Generic, NFDataX)

{-# ANN module (
  DataReprAnn
    $(liftQ [t|Box|])
    6
    [ ConstrRepr 'Box 0b1111_00 0b0101_00 [0b0000_11] ] ) #-}

deriveBitPack [t|Box|]

topEntity :: Signal System (BitVector 2) -> Signal System Box
topEntity = fmap Box
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput = stimuliGenerator clk rst $ 0b00
                                        :> 0b01
                                        :> 0b10
                                        :> 0b11
                                        :> Nil

    expectedOutput = outputVerifierBitVector' clk rst $ 0b0101_00
                                                     :> 0b0101_01
                                                     :> 0b0101_10
                                                     :> 0b0101_11
                                                     :> Nil

    done = expectedOutput (pack <$> topEntity testInput)
    clk  = tbSystemClockGen (not <$> done)
    rst  = systemResetGen
