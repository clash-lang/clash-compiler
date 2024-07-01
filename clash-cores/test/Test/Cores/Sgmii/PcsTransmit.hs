{-# LANGUAGE ViewPatterns #-}

module Test.Cores.Sgmii.PcsTransmit where

import Clash.Cores.Sgmii.PcsTransmit
import qualified Clash.Prelude as C
import qualified Hedgehog as H
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Prelude

-- | The following property is only really used to check whether the behaviour
--   of the block stays consistent with previous behaviour if any changes are
--   made to the structure of the state machine.
prop_pcsTransmit :: H.Property
prop_pcsTransmit = H.withTests 1 $ H.property $ do
  let dut (C.unbundle -> (txEn, txEr, dw, xmit, txConfReg)) =
        pcsTransmit @C.System txEn txEr dw xmit txConfReg

      inp = []

      simOut = C.sampleN (length inp) (dut (C.fromList inp))

      expected = []

  simOut H.=== expected

tests :: TestTree
tests = $(testGroupGenerator)
