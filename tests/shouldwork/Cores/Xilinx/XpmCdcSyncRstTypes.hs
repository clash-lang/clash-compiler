{-# LANGUAGE BangPatterns #-}

module XpmCdcSyncRstTypes where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import Data.Proxy
import Language.Haskell.TH.Lib

import Clash.Cores.Xilinx.Xpm.Cdc.SyncRst
import XpmTestCommon

randomRstSrc :: KnownDomain dom => Clock dom -> Reset dom
randomRstSrc clk = unsafeFromActiveHigh $ genTestData randomSeed clk

tb ::
  forall a b stages n .
  ( KnownNat n, 1 <= n
  , KnownNat stages, 2 <= stages, stages <= 10
  , KnownDomain a
  , KnownDomain b
  ) =>
  Proxy a -> Proxy b ->
  -- | Initial values
  Maybe Bool ->
  SNat stages ->
  -- | Expected data
  Vec n (BitVector 1) ->
  Signal b Bool
tb Proxy Proxy initVals SNat expectedDat = done
 where
  actual =
    xpmCdcSyncRstWith
      @stages
      (XpmCdcSyncRstConfig (SNat @stages) initVals)
      clkA clkB (randomRstSrc clkA)

  done =
    outputVerifierWith
      (\clk rst -> assertBitVector clk rst $(lift $ "outputVerifier (seed:" <> show randomSeed <> ")"))
      clkB clkB noReset
      expectedDat
      (pack <$> unsafeToActiveHigh actual)
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
  Maybe Bool ->
  SNat stages ->
  SNat samples ->
  ExpQ
expected Proxy Proxy initVals SNat SNat = listToVecTH out1
 where
  out0 = unsafeToActiveHigh $
    xpmCdcSyncRstWith
      @stages
      (XpmCdcSyncRstConfig (SNat @stages) initVals)
      clkA
      clkB
      (randomRstSrc clkA)
  clkA = clockGen @a
  clkB = clockGen @b

  out1 = pack <$> sampleN (natToNum @samples) out0
