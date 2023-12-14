{-# LANGUAGE BangPatterns #-}

module XpmCdcPulseTypes where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import Data.Proxy
import Language.Haskell.TH.Lib

import Clash.Cores.Xilinx.Xpm.Cdc.Pulse
import XpmTestCommon

testData :: KnownDomain dom => Clock dom -> Signal dom (Unsigned 1)
testData clk = genTestData (randomSeed+1) clk

randomRstSrc :: KnownDomain dom => Clock dom -> Reset dom
randomRstSrc clk = unsafeFromActiveHigh $ genTestData (randomSeed+2) clk

randomRstDst :: KnownDomain dom => Clock dom -> Reset dom
randomRstDst clk = unsafeFromActiveHigh $ genTestData (randomSeed+3) clk

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
  -- | Registered output
  Bool ->
  -- | Resets used
  Bool ->
  SNat stages ->
  -- | Expected data
  Vec n (BitVector 1) ->
  Signal b Bool
tb Proxy Proxy initVals regOutput rstUsed SNat expectedDat = done
 where
  actual =
    xpmCdcPulseWith
      @stages @(Unsigned 1)
      (XpmCdcPulseConfig (SNat @stages) initVals rstUsed regOutput)
      clkA rstA clkB rstB (testData clkA)

  done =
    outputVerifierWith
      (\clk rst -> assertBitVector clk rst $(lift $ "outputVerifier (seed:" <> show randomSeed <> ")"))
      clkB clkB noReset
      expectedDat
      (pack <$> actual)
  rstA = randomRstSrc clkA
  rstB = unsafeFromActiveHigh $ unsafeSynchronizer clkA clkB $ unsafeToActiveHigh rstA

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
  -- | Registered output
  Bool ->
  -- | Resets used
  Bool ->
  SNat stages ->
  SNat samples ->
  ExpQ
expected Proxy Proxy initVals regOutput rstUsed SNat SNat = listToVecTH out1
 where
  out0 =
    xpmCdcPulseWith
      @stages @(Unsigned 1)
      (XpmCdcPulseConfig (SNat @stages) initVals regOutput rstUsed)
      clkA rstA
      clkB rstB
      (testData clockGen)
  clkA = clockGen @a
  clkB = clockGen @b
  rstA = randomRstSrc clkA
  rstB = unsafeFromActiveHigh $ unsafeSynchronizer clkA clkB $ unsafeToActiveHigh rstA

  out1 = pack <$> sampleN (natToNum @samples) out0
