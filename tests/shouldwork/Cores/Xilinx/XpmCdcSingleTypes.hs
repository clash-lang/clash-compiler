{-# LANGUAGE BangPatterns #-}

module XpmCdcSingleTypes where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import Data.Proxy
import Language.Haskell.TH.Lib

import Clash.Cores.Xilinx.Xpm.Cdc.Single

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
  counter = delay clkA enableGen 0 (counter + 1)

  actual =
    xpmCdcSingleWith
      @stages @(Unsigned 1)
      (XpmCdcSingleConfig SNat initVals regInput)
      clkA clkB counter

  done =
    outputVerifierWith
      (\clk rst -> assertBitVector clk rst "outputVerifier")
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
    xpmCdcSingleWith
      @stages @(Unsigned 1)
      (XpmCdcSingleConfig SNat initVals regInput)
      (clockGen @a)
      (clockGen @b)
      (fromList [0..])

  out1 = pack <$> sampleN (natToNum @samples) out0
