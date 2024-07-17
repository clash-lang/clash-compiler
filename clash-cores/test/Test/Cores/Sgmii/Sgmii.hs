{-# LANGUAGE ViewPatterns #-}

module Test.Cores.Sgmii.Sgmii where

import Clash.Cores.Sgmii
import Clash.Cores.Sgmii.Common
import Clash.Hedgehog.Sized.BitVector
import qualified Clash.Prelude as C
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Cores.Sgmii.AutoNeg
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Prelude

-- | Placeholder integration function for all different parts of SGMII
sgmiiSim ::
  (C.HiddenClockResetEnable dom) =>
  C.Signal dom Bool ->
  C.Signal dom Bool ->
  C.Signal dom (C.BitVector 8) ->
  C.Signal dom (C.BitVector 10) ->
  ( C.Signal dom Bool
  , C.Signal dom Bool
  , C.Signal dom (C.BitVector 8)
  , C.Signal dom (C.BitVector 10)
  )
sgmiiSim txEn txEr txDw rxCg = (rxDv, rxEr, rxDw, txCg)
 where
  txCg = sgmiiTx txEn txEr txDw xmit txConfReg (pure Nothing)
  (_, rxDv, rxEr, rxDw, xmit, txConfReg, _, _) = sgmiiRx rxCg

-- | Loopback function that combines two full SGMII systems, with one in
--   loopback configuration, to check whether the full system including
--   auto-negotiation works
loopbackSim ::
  (C.HiddenClockResetEnable dom) =>
  C.Signal dom (Bool, Bool, C.BitVector 8) ->
  C.Signal dom (C.BitVector 8)
loopbackSim (C.unbundle -> (txEn, txEr, txDw)) = rxDw
 where
  (_, _, rxDw, txCg) = sgmiiSim txEn txEr txDw rxCg
  (dv, er, dw, rxCg) = sgmiiSim dv er dw txCg

-- | Function that runs two versions of 'sgmii' at the same time
duplexTransmissionSim ::
  (C.HiddenClockResetEnable dom) =>
  C.Signal dom (Bool, Bool, C.BitVector 8, C.BitVector 8) ->
  C.Signal dom (C.BitVector 8, C.BitVector 8)
duplexTransmissionSim (C.unbundle -> (txEn, txEr, txDw1, txDw2)) =
  C.bundle (rxDw1, rxDw2)
 where
  (_, _, rxDw1, cg1) = sgmiiSim txEn txEr txDw1 cg2
  (_, _, rxDw2, cg2) = sgmiiSim txEn txEr txDw2 cg1

-- | Function that is used to propagate the config register via 'pcsTransmit'
--   'pcsReceive'
confRegSim ::
  (C.HiddenClockResetEnable dom) =>
  C.Signal dom ConfReg ->
  C.Signal dom ConfReg
confRegSim txConfReg = fromMaybe 0 <$> rxConfReg
 where
  (_, _, _, _, _, _, rxConfReg, _) = sgmiiRx txCg
  txCg =
    sgmiiTx
      (pure False)
      (pure False)
      (pure 0)
      (pure (Just Conf))
      (Just <$> txConfReg)
      (pure Nothing)

-- | Test that the completely integrated system works in a loopback mode, where
--   the output of the second 'sgmii' instance is connected to its own input.
--   The system starts in configuration mode, so the first 76 samples are used
--   to set it up fully, after which it is ready for data transmission. During
--   data transmission, the first several packages are replaced with control
--   packages, these are dropped from the output comparision. Then, the output
--   is shifted to deal with the delay that is introduced along the way due to
--   registers.
prop_loopbackTest :: H.Property
prop_loopbackTest = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 1 100))

  inp <- H.forAll (Gen.list (Range.singleton simDuration) genDefinedBitVector)
  let setupSamples = 77
      delaySamples = 20
      controlCount = 9

      simOut =
        drop (setupSamples + delaySamples + controlCount) $
          C.sampleN
            (simDuration + setupSamples + delaySamples)
            ( loopbackSim @C.System
                ( C.fromList
                    ( replicate setupSamples (False, False, 0)
                        ++ map f inp
                        ++ replicate delaySamples (False, False, 0)
                    )
                )
            )
       where
        f dw = (True, False, dw)

      expected = drop controlCount inp

  simOut H.=== expected

-- | Similar to 'prop_loopbackTest', however this time there is no loopback but
--   different signals are sent from left to right and right to left
prop_duplexTransmission :: H.Property
prop_duplexTransmission = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 1 100))

  inp1 <- H.forAll (Gen.list (Range.singleton simDuration) genDefinedBitVector)
  inp2 <- H.forAll (Gen.list (Range.singleton simDuration) genDefinedBitVector)
  let setupSamples = 77
      delaySamples = 10
      controlCount = 9

      inp = zip inp1 inp2

      simOut =
        drop (setupSamples + delaySamples + controlCount) $
          C.sampleN
            (simDuration + setupSamples + delaySamples)
            ( duplexTransmissionSim @C.System
                ( C.fromList
                    ( replicate setupSamples (False, False, 0, 0)
                        ++ map f inp
                        ++ replicate delaySamples (False, False, 0, 0)
                    )
                )
            )
       where
        f (dw1, dw2) = (True, False, dw1, dw2)

      expected = map swap $ drop controlCount inp

  simOut H.=== expected

-- | Similar to 'prop_duplexTransmission', however this time the system enters
--   carrier extend and starts retransmission
prop_duplexTransmissionCarrierExtend :: H.Property
prop_duplexTransmissionCarrierExtend = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 10 100))

  inp1 <- H.forAll (Gen.list (Range.singleton simDuration) genDefinedBitVector)
  inp2 <- H.forAll (Gen.list (Range.singleton simDuration) genDefinedBitVector)
  inp3 <- H.forAll (Gen.list (Range.singleton simDuration) genDefinedBitVector)
  inp4 <- H.forAll (Gen.list (Range.singleton simDuration) genDefinedBitVector)
  let setupSamples = 77
      delaySamples = 10
      controlCount = 9
      extendCount = 2

      inpA = zip inp1 inp2
      inpB = zip inp3 inp4

      simOut =
        drop (setupSamples + delaySamples + controlCount) $
          C.sampleN
            (2 * simDuration + setupSamples + delaySamples + extendCount)
            ( duplexTransmissionSim @C.System
                ( C.fromList
                    ( replicate setupSamples (False, False, 0, 0)
                        ++ map f inpA
                        ++ [(False, True, 0, 0), (False, True, 0, 0)]
                        ++ map f inpB
                        ++ replicate delaySamples (False, False, 0, 0)
                    )
                )
            )
       where
        f (dw1, dw2) = (True, False, dw1, dw2)

      expected1 = map swap $ drop controlCount inpA
      simOut1 = take (simDuration - controlCount) simOut

      expected2 = map swap $ drop controlCount inpB
      simOut2 =
        take (simDuration - controlCount) $
          drop (simDuration + extendCount) simOut

  simOut1 H.=== expected1
  simOut2 H.=== expected2

-- | Assert that the configuration register will be propagated from the transmit
--   block to the receive block in exactly 25 cycles
prop_confRegPropagated :: H.Property
prop_confRegPropagated = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.singleton 25))

  inp <- H.forAll genConfRegNoAck
  let simOut =
        drop 1 $ C.sampleN (simDuration + 1) (confRegSim @C.System (pure inp))

  H.assert $ inp `elem` simOut

tests :: TestTree
tests = $(testGroupGenerator)
