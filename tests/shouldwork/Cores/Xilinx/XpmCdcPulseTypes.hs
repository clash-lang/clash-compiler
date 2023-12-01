{-# LANGUAGE BangPatterns #-}

module XpmCdcPulseTypes where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import Data.Proxy
import Language.Haskell.TH.Lib

import Clash.Cores.Xilinx.Xpm.Cdc.Pulse
import XpmTestCommon

testData :: KnownDomain dom => Clock dom -> Signal dom (Unsigned 1)
testData clk = conditionTestData $ genTestData randomSeed clk
 where
  conditionTestData = id

tb ::
  forall a b stages n .
  ( KnownNat n, 1 <= n
  , KnownNat stages, 2 <= stages, stages <= 10
  , KnownDomain a
  , KnownDomain b
  ) =>
  Proxy a -> Proxy b ->
  -- | Initial values
  Bool ->
  -- | Registered input
  Bool ->
  SNat stages ->
  -- | Expected data
  Vec n (BitVector 1) ->
  Signal b Bool
tb Proxy Proxy initVals regInput SNat expectedDat = done
 where
  actual =
    xpmCdcPulseWith
      @stages @(Unsigned 1)
      (XpmCdcPulseConfig SNat initVals regInput)
      clkA clkB (testData clkA)

  done =
    outputVerifierWith
      (\clk rst -> assertBitVector clk rst $(lift $ "outputVerifier (seed:" <> show randomSeed <> ")"))
      clkB clkB noReset
      expectedDat
      (pack <$> actual)

  -- Testbench clocks
  clkA :: Clock a
  clkA = tbClockGen (not <$> unsafeSynchronizer clkB clkA done)
  clkB :: Clock b
  clkB = tbClockGen (not <$> done)

expected ::
  forall a b stages samples .
  ( KnownDomain a
  , KnownDomain b
  , 2 <= stages, stages <= 10
  ) =>
  Proxy a ->
  Proxy b ->
  -- | Initial values
  Bool ->
  -- | Registered input
  Bool ->
  SNat stages ->
  SNat samples ->
  ExpQ
expected Proxy Proxy initVals regInput SNat SNat = listToVecTH out1
 where
  out0 =
    xpmCdcPulseWith
      @stages @(Unsigned 1)
      (XpmCdcPulseConfig SNat initVals regInput)
      (clockGen @a)
      (clockGen @b)
      (testData clockGen)

  out1 = pack <$> sampleN (natToNum @samples) out0
