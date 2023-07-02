{-# LANGUAGE CPP #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module BitPackDerivation where

import Clash.Prelude
import Clash.Prelude.Testbench
import Clash.Annotations.BitRepresentation.Deriving

type SmallInt = Unsigned 2

data Train
  = Passenger
      SmallInt
      -- ^ Number of wagons
  | Freight
      ( SmallInt
      -- ^ Number of wagons
      , SmallInt )
      -- ^ Max weight
  | Maintenance
  | Toy

deriveAnnotation (simpleDerivator OneHot OverlapL) [t| Train |]
deriveBitPack [t| Train |]

topEntity
  :: SystemClockResetEnable
  => Signal System Train
  -> Signal System (BitVector 8)
topEntity trains = pack <$> trains
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench
  :: Signal System Bool
testBench = done'
  where
    testInput :: _ => _
    testInput = stimuliGenerator $ Toy
                                :> Maintenance
                                :> Freight (2, 3)
                                :> Passenger 1
                                :> Nil

    expectedOutput :: SystemClockResetEnable
                   => Signal System (BitVector 8) -> Signal System Bool
    expectedOutput = outputVerifierBitVector' $ $(bLit "1000....")
                                             :> $(bLit "0100....")
                                             :> $(bLit "00101011")
                                             :> $(bLit "000101..")
                                             :> Nil
    done :: _ => _
    done  = expectedOutput (topEntity testInput)
    done' =
      withClockResetEnable
        (tbSystemClockGen (not <$> done'))
        systemResetGen
        enableGen
        done
