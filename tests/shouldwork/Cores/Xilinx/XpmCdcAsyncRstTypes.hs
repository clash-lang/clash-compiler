{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module XpmCdcAsyncRstTypes where

import Clash.Cores.Xilinx.Xpm.Cdc.AsyncRst (xpmCdcAsyncRstWith, XpmCdcAsyncRstConfig(XpmCdcAsyncRstConfig))
import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Data.Proxy
import Language.Haskell.TH.Lib

createDomain vXilinxSystem{vName="D3",  vPeriod=hzToPeriod 30e6}
createDomain vXilinxSystem{vName="D5",  vPeriod=hzToPeriod 50e6}

main :: IO ()
main = print $ sampleN 24 $ inputFsm d3 (clockGen @XilinxSystem)

-- | Enumerates all possible values [0..2^n-1] as bits. Will wrap around after
-- @n * 2^n@ cycles.
--
-- >>> sampleN 8 $ input d2 (clockGen @XilinxSystem)
-- [0,0,0,1,1,0,1,1]
--
-- TODO?: undefined values
--
inputFsm ::
  forall n dom .
  KnownDomain dom =>
  SNat n ->
  Clock dom ->
  Signal dom Bit
inputFsm SNat clk =
  moore
    clk noRst enableGen
    goT goO
    (0 :: Unsigned n, maxBound :: Index n, 0 :: Unsigned n)
    (pure ())
 where
  goO (_,   _, n2) = msb n2

  goT (n0, 0,  _ ) () = (n0 + 1, maxBound, n0 + 1     )
  goT (n0, n1, n2) () = (n0, n1 - 1,       shiftL n2 1)

type GetNCycles stages = 2 * (2 * stages) * (2^(2 * stages))

noRst :: KnownDomain dom => Reset dom
noRst = unsafeFromHighPolarity (pure False)

tb ::
  forall a b stages .
  ( KnownNat stages, 2 <= stages, stages <= 10
  , 1 <= GetNCycles stages
  , KnownDomain a
  , KnownDomain b
  ) =>
  Proxy a -> Proxy b ->
  -- | Initial values
  Bool ->
  -- | Number of synchronization stages
  SNat stages ->
  -- | Expected data
  Vec (GetNCycles stages) (BitVector 1) ->
  Signal b Bool
tb Proxy Proxy initVals stages@SNat expectedDat = done
 where
  input :: Signal a Bit
  input = inputFsm (SNat @(2 * stages)) clkA

  actual :: Reset b
  actual =
    xpmCdcAsyncRstWith
      @stages
      (XpmCdcAsyncRstConfig stages initVals)
      clkA clkB
      (unsafeFromHighPolarity $ bitToBool <$> input)

  done =
    outputVerifierWith
      (\clk rst -> assertBitVector clk rst "outputVerifier Port A")
      clkB clkB (noRst @b)
      expectedDat
      (pack <$> unsafeToHighPolarity actual)

  -- Testbench clocks
  clkA :: Clock a
  clkA = tbClockGen (not <$> unsafeSynchronizer clkB clkA done)
  clkB :: Clock b
  clkB = tbClockGen (not <$> done)

expected ::
  forall a b stages .
  ( KnownDomain a
  , KnownDomain b
  , 2 <= stages, stages <= 10
  ) =>
  Proxy a ->
  Proxy b ->
  -- | Initial values
  Bool ->
  SNat stages ->
  ExpQ
expected Proxy Proxy initVals stages@SNat = listToVecTH out1
 where
  out0 =
    xpmCdcAsyncRstWith
      @stages
      (XpmCdcAsyncRstConfig stages initVals)
      (clockGen @a)
      (clockGen @b)
      (unsafeFromHighPolarity $ bitToBool <$> inputFsm (SNat @(2 * stages)) clockGen)

  out1 = pack <$> sampleN (natToNum @(GetNCycles stages)) (unsafeToHighPolarity out0)
