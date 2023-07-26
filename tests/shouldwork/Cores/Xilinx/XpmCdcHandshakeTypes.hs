{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module XpmCdcHandshakeTypes where

import Clash.Cores.Xilinx.Xpm.Cdc.Handshake
import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Data.Proxy
import Language.Haskell.TH.Lib

createDomain vXilinxSystem{vName="D3",  vPeriod=hzToPeriod 30e6}
createDomain vXilinxSystem{vName="D5",  vPeriod=hzToPeriod 50e6}
createDomain vXilinxSystem{vName="D10", vPeriod=hzToPeriod 100e6}
createDomain vXilinxSystem{vName="D11", vPeriod=hzToPeriod 110e6}

data State = WaitForDeassert | WaitForAssert deriving (Generic, NFDataX)

-- | Transfer 1, 2, 3, ... to destination domain
srcFsm ::
  forall a src .
  ( KnownDomain src
  , Num a
  , NFDataX a
  ) =>
  Clock src ->
  Signal src Bool ->
  Signal src (a, Bool)
srcFsm clk = mealy clk noReset enableGen go (0, WaitForDeassert)
 where
  go (n, WaitForDeassert) True  = ((n,     WaitForDeassert), (n,     False))
  go (n, WaitForDeassert) False = ((n + 1, WaitForAssert),   (n + 1, True))
  go (n, WaitForAssert)   False = ((n,     WaitForAssert),   (n,     True))
  go (n, WaitForAssert)   True  = ((n,     WaitForDeassert), (n,     False))
{-# NOINLINE srcFsm #-}

-- | Receives data from source domain
dstFsm ::
  forall a dst .
  KnownDomain dst =>
  Clock dst ->
  Signal dst (Bool, a) ->
  Signal dst (Bool, Maybe a)
dstFsm clk = mealy clk noReset enableGen go WaitForAssert
 where
  go WaitForAssert   (False, _) = (WaitForAssert,   (False, Nothing))
  go WaitForAssert   (True,  n) = (WaitForDeassert, (True,  Just n))
  go WaitForDeassert (True,  _) = (WaitForDeassert, (True,  Nothing))
  go WaitForDeassert (False, _) = (WaitForAssert,   (False, Nothing))
{-# NOINLINE dstFsm #-}

-- | Composition of 'srcFsm' and 'dstFsm'
top ::
  forall a srcStages dstStages src dst .
  ( KnownDomain src
  , KnownDomain dst
  , Num a
  , NFDataX a
  , BitPack a
  , 1 <= BitSize a, BitSize a <= 1024
  , 2 <= srcStages, srcStages <= 10
  , 2 <= dstStages, dstStages <= 10
  ) =>
  XpmCdcHandshakeConfig srcStages dstStages ->
  Clock src ->
  Clock dst ->
  Signal dst (Maybe a)
top opts clkSrc clkDst = maybeDat
 where
  (srcIn, srcSend) = unbundle $ srcFsm @a clkSrc srcRcv

  (destOut, destReq, srcRcv) =
    xpmCdcHandshakeWith opts clkSrc clkDst srcIn srcSend destAck

  (destAck, maybeDat) =
    unbundle $ dstFsm @a clkDst $ bundle (destReq, destOut)
{-# NOINLINE top #-}

tb ::
  forall a b srcStages dstStages n .
  ( KnownNat n, 1 <= n
  , 2 <= srcStages, srcStages <= 10
  , 2 <= dstStages, dstStages <= 10
  , KnownDomain a
  , KnownDomain b
  ) =>
  Proxy a -> Proxy b ->
  XpmCdcHandshakeConfig srcStages dstStages ->
  -- | Expected data
  Vec n (Maybe (Unsigned 8)) ->
  Signal b Bool
tb Proxy Proxy opts expectedDat = done
 where
  actual = top @(Unsigned 8) opts clkA clkB

  done = outputVerifier' clkB noReset expectedDat actual

  -- Testbench clocks
  clkA :: Clock a
  clkA = tbClockGen (not <$> unsafeSynchronizer clkB clkA done)
  clkB :: Clock b
  clkB = tbClockGen (not <$> done)

expected ::
  forall a b srcStages dstStages samples .
  ( KnownDomain a
  , KnownDomain b
  , 2 <= srcStages, srcStages <= 10
  , 2 <= dstStages, dstStages <= 10
  ) =>
  Proxy a ->
  Proxy b ->
  XpmCdcHandshakeConfig srcStages dstStages ->
  SNat samples ->
  ExpQ
expected Proxy Proxy opts SNat = listToVecTH out1
 where
  out0 = top @(Unsigned 8) opts (clockGen @a) (clockGen @b)
  out1 = sampleN (natToNum @samples) out0
