{-# LANGUAGE BangPatterns #-}

module XpmCdcGrayTypes where

import Clash.Cores.Xilinx.Xpm.Cdc.Gray
import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Data.Proxy
import Language.Haskell.TH.Lib

createDomain vXilinxSystem{vName="D3",  vPeriod=hzToPeriod 30e6}
createDomain vXilinxSystem{vName="D5",  vPeriod=hzToPeriod 50e6}
createDomain vXilinxSystem{vName="D10", vPeriod=hzToPeriod 100e6}
createDomain vXilinxSystem{vName="D11", vPeriod=hzToPeriod 110e6}

noRst :: KnownDomain dom => Reset dom
noRst = unsafeFromHighPolarity (pure False)

tb ::
  forall a b width stages n .
  ( KnownNat n, 1 <= n
  , KnownNat stages, 2 <= stages, stages <= 10
  , KnownNat width, 2 <= width, width <= 32
  , KnownDomain a
  , KnownDomain b
  ) =>
  Proxy a -> Proxy b -> SNat stages ->
  -- | Expected data
  Vec n (BitVector width) ->
  Signal b Bool
tb Proxy Proxy SNat expectedDat = done
 where
  counter = delay clkA enableGen 0 (counter + 1)

  actual = xpmCdcGrayWith @stages @width (XpmCdcGrayConfig SNat True) clkA clkB counter

  done =
    outputVerifierWith
      (\clk rst -> assertBitVector clk rst "outputVerifier Port A")
      clkB clkB (noRst @b)
      expectedDat
      (pack <$> actual)

  -- Testbench clocks
  clkA :: Clock a
  clkA = tbClockGen (not <$> unsafeSynchronizer clkB clkA done)
  clkB :: Clock b
  clkB = tbClockGen (not <$> done)

expected ::
  forall a b n stages samples .
  ( KnownDomain a
  , KnownDomain b
  , 2 <= stages, stages <= 10
  , 2 <= n, n <= 32
  ) =>
  Proxy a ->
  Proxy b ->
  SNat n ->
  SNat stages ->
  SNat samples ->
  ExpQ
expected Proxy Proxy SNat SNat SNat = listToVecTH out1
 where
  out0 =
    xpmCdcGrayWith
      @stages @n
      (XpmCdcGrayConfig SNat True)
      (clockGen @a)
      (clockGen @b)
      (fromList [0..])

  out1 = pack <$> sampleN (natToNum @samples) out0
