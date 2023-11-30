{-# LANGUAGE BangPatterns #-}

module XpmCdcArraySingleTypes where

import Clash.Cores.Xilinx.Xpm.Cdc.ArraySingle
import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Data.Proxy
import Language.Haskell.TH.Lib

tb ::
  forall a b stages width n .
  ( KnownNat n, 1 <= n
  , KnownNat stages, 2 <= stages, stages <= 10
  , KnownNat width, 1 <= width, width <= 1024
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
  Vec n (BitVector width) ->
  Signal b Bool
tb Proxy Proxy initVals regInput SNat expectedDat = done
 where
  counter = delay clkA enableGen 0 (counter + 1)

  actual =
    xpmCdcArraySingleWith
      @stages @(Unsigned width)
      (XpmCdcArraySingleConfig SNat initVals regInput)
      clkA
      clkB
      counter

  done =
    outputVerifierWith
      (\clk rst -> assertBitVector clk rst "outputVerifier A")
      clkB clkB noReset
      expectedDat
      (pack <$> actual)

  -- Testbench clocks
  clkA :: Clock a
  clkA = tbClockGen (not <$> unsafeSynchronizer clkB clkA done)
  clkB :: Clock b
  clkB = tbClockGen (not <$> done)

expected ::
  forall a b stages width samples .
  ( KnownDomain a
  , KnownDomain b
  , 2 <= stages, stages <= 10
  , 1 <= width, width <= 1024
  ) =>
  Proxy a ->
  Proxy b ->
  -- | Initial values
  Bool ->
  -- | Registered input
  Bool ->
  SNat stages ->
  SNat width ->
  SNat samples ->
  ExpQ
expected Proxy Proxy initVals regInput SNat SNat SNat = listToVecTH out1
 where
  out0 =
    xpmCdcArraySingleWith
      @stages @(Unsigned width)
      (XpmCdcArraySingleConfig SNat initVals regInput)
      (clockGen @a)
      (clockGen @b)
      (fromList [0..])

  out1 = pack <$> sampleN (natToNum @samples) out0
