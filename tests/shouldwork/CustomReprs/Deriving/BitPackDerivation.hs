{-# LANGUAGE InstanceSigs #-}
module BitPackDerivation where

import Train
import Clash.Prelude.Testbench
import Clash.Prelude
import Clash.Sized.Internal.BitVector
import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving

deriveAnnotation (simpleDerivators OneHot Overlap) [t| Train |]
deriveBitPack [t| Train |]

topEntity
  :: SystemClockReset
  => Signal System Train
  -> Signal System (BitVector 8)
topEntity trains = pack <$> trains
{-# NOINLINE topEntity #-}

testBench
--   :: SystemClockReset
  :: Signal System Bool
testBench = done'
  where
    testInput = stimuliGenerator $ Toy
                                :> Maintenance
                                :> Freight 2 3
                                :> Passegner 1
                                :> Nil

    expectedOutput = outputVerifier $ 0b10000000
                                   :> 0b01000000
                                   :> 0b00101011
                                   :> 0b00010100
                                   :> Nil
    done  = expectedOutput (topEntity testInput)
    done' = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done

