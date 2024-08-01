module Test.Cores.Sgmii.Sync where

import Clash.Cores.LineCoding8b10b
import Clash.Cores.Sgmii.Common
import Clash.Cores.Sgmii.Sync
import Clash.Hedgehog.Sized.BitVector
import qualified Clash.Prelude as C
import Data.Function (on)
import Data.List (group, mapAccumL, maximumBy)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Cores.LineCoding8b10b
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Prelude

-- | Simulation function for 'sync' that provides a bundled output
syncSim ::
  (C.HiddenClockResetEnable dom) =>
  C.Signal dom Cg ->
  C.Signal dom (Cg, Bool, C.Vec 3 Symbol8b10b, Even, Status)
syncSim cg = C.bundle $ sync cg

-- | Run the 'sync' function on a list of values that do not contain any comma
--   code groups and assert that the 'SyncStatus' will never go to 'Ok'
prop_syncNotOk :: H.Property
prop_syncNotOk = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 1 100))

  inp <-
    H.forAll
      ( Gen.list
          (Range.singleton simDuration)
          (Gen.filter checkBitSequence genDefinedBitVector)
      )
  let simOut =
        map f $
          C.sampleN (simDuration + 1) (syncSim @C.System (C.fromList (0 : inp)))
       where
        f (_, _, _, _, syncStatus) = syncStatus

  H.assert (Ok `notElem` simOut)

-- | Check that for any given input data word, this data word will always be
--   propagated to the output of the 'sync' block
prop_syncPropagateDw :: H.Property
prop_syncPropagateDw = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 1 100))

  inp <-
    H.forAll
      ( Gen.list
          (Range.singleton simDuration)
          (Gen.filter checkBitSequence genDefinedBitVector)
      )
  let delaySamples = 4

      simOut =
        map f $
          drop (1 + delaySamples) $
            C.sampleN
              (simDuration + 1)
              (syncSim @C.System (C.fromList (0 : inp)))
       where
        f (_, _, dw, _, _) = C.head dw

      expected =
        take (simDuration - delaySamples) $
          snd (mapAccumL decode8b10b False inp)

  simOut H.=== expected

-- | Assert that 'Even' is never two times 'Odd' in a row, and that 'Even'
--   is never the same more than two times in a row
prop_syncCheckEven :: H.Property
prop_syncCheckEven = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 1 100))

  inp <- H.forAll (Gen.list (Range.singleton simDuration) genDefinedBitVector)
  let delaySamples = 4

      simOut =
        map f $
          drop delaySamples $
            C.sampleN
              (simDuration + delaySamples)
              (syncSim @C.System (C.fromList (replicate delaySamples 0 ++ inp)))
       where
        f (_, _, _, rxEven, _) = rxEven

  H.assert $ [Odd, Odd] `notElem` group simOut
  H.assert $ length (maximumBy (compare `on` length) (group simOut)) < 3

tests :: TestTree
tests = $(testGroupGenerator)
