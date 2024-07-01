{-# LANGUAGE ViewPatterns #-}

module Test.Cores.Sgmii.PcsReceive where

import Clash.Cores.LineCoding8b10b
import Clash.Cores.Sgmii.Common
import Clash.Cores.Sgmii.PcsReceive
import Clash.Hedgehog.Sized.BitVector
import qualified Clash.Prelude as C
import Data.List (find)
import Data.Maybe (isJust)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Prelude

-- | Function that creates a list with a given range that contains a list of
--   running disparities and 'Symbol8b10b's
genSymbol8b10bs :: H.Range Int -> H.Gen [(Bool, Symbol8b10b)]
genSymbol8b10bs range = do
  n <- Gen.int range
  genSymbol8b10bs1 n False

-- | Recursive function to generate a list of 'Symbol8b10b's with the correct
--   running disparity
genSymbol8b10bs1 :: Int -> Bool -> H.Gen [(Bool, Symbol8b10b)]
genSymbol8b10bs1 0 _ = pure []
genSymbol8b10bs1 n rd = do
  (rdNew, dw) <- genSymbol8b10b rd
  ((rdNew, dw) :) <$> genSymbol8b10bs1 (pred n) rdNew

-- | Generate a 'Symbol8b10b' by creating a 'BitVector' of length 10 and
--   decoding it with the 'decode8b10b' function
genSymbol8b10b :: Bool -> H.Gen (Bool, Symbol8b10b)
genSymbol8b10b rd = Gen.filter f $ decode8b10b rd <$> genDefinedBitVector
 where
  f (_, dw) = isDw dw

-- | Version of 'pcsReceive' that does not return any actual values, but only
--   the entered state for debugging purposes.
pcsReceiveSim ::
  (C.HiddenClockResetEnable dom) =>
  PcsReceiveState ->
  C.Signal
    dom
    ( C.BitVector 10
    , Bool
    , C.Vec 3 Symbol8b10b
    , Even
    , SyncStatus
    , Maybe Xmit
    ) ->
  C.Signal dom PcsReceiveState
pcsReceiveSim s i = s'
 where
  (s', _, _, _, _, _) =
    C.unbundle $ C.mealy pcsReceiveT s i

-- | Test that for an arbitrary list of inputs 'Symbol8b10b's, the state machine
--   will move from @START_OF_PACKET@ to @RX_DATA@
prop_pcsReceiveStartOfPacket :: H.Property
prop_pcsReceiveStartOfPacket = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 10 100))

  inp <- H.forAll (genSymbol8b10bs (Range.singleton simDuration))

  let simOut =
        C.sampleN
          simDuration
          ( pcsReceiveSim @C.System
              (StartOfPacket True Idle)
              (C.fromList (map f inp))
          )
       where
        f (rd, dw) = (0, rd, C.repeat dw, Even, Ok, Just Idle)

  H.assert $ isJust $ find g simOut
 where
  g RxData{} = True
  g _ = False

tests :: TestTree
tests = $(testGroupGenerator)
