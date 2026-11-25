{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A field annotated as zero-width by a custom bit representation.
-- `deriveBitPack` used to reject this, as the generated `unpack` selects no
-- bit ranges at all for such a field.
module ZeroWidthFieldAnn where

import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving
import Clash.Explicit.Testbench
import Clash.Prelude

data Box = Box (BitVector 2) ()
  deriving (Generic, NFDataX)

{-# ANN module (
  DataReprAnn
    $(liftQ [t|Box|])
    6
    [ ConstrRepr 'Box 0b1111_00 0b0101_00 [0b0000_11, 0b0000_00] ] ) #-}

deriveBitPack [t|Box|]

-- | 'unpack' has to conjure the `()` back out of no bits at all.
roundTrip :: Box -> BitVector 2
roundTrip b = case unpack (pack b) of Box x () -> x

topEntity :: Signal System (BitVector 2) -> Signal System (Box, BitVector 2)
topEntity = fmap (\x -> let b = Box x () in (b, roundTrip b))
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput = stimuliGenerator clk rst $ 0b00
                                        :> 0b01
                                        :> 0b10
                                        :> 0b11
                                        :> Nil

    expectedOutput = outputVerifierBitVector' clk rst $ 0b0101_00_00
                                                     :> 0b0101_01_01
                                                     :> 0b0101_10_10
                                                     :> 0b0101_11_11
                                                     :> Nil

    done = expectedOutput (pack <$> topEntity testInput)
    clk  = tbSystemClockGen (not <$> done)
    rst  = systemResetGen
