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

data State = WaitForDeassert | WaitForAssert (Index 2) deriving (Generic, NFDataX)

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
  go (n, WaitForDeassert) True  = ((n,     WaitForDeassert),         (0,     False))
  go (n, WaitForDeassert) False = ((n + 1, WaitForAssert maxBound),  (n + 1, True))
  go (n, WaitForAssert _) False = ((n,     WaitForAssert maxBound),  (n,     True))
  go (n, WaitForAssert 0) True  = ((n,     WaitForDeassert),         (0,     False))
  go (n, WaitForAssert w) True  = ((n,     WaitForAssert (w-1)),     (n,     True))  -- seen src_rcv, wait a little before dropping src_send
{-# NOINLINE srcFsm #-}

-- | Receives data from source domain
dstFsm ::
  forall a dst .
  KnownDomain dst =>
  Clock dst ->
  Signal dst (Bool, a) ->
  Signal dst (Bool, Maybe a)
dstFsm clk = mealy clk noReset enableGen go (WaitForAssert maxBound)
 where
  go (WaitForAssert _)  (False, _) = (WaitForAssert maxBound,   (False, Nothing))
  go (WaitForAssert 0)  (True,  n) = (WaitForDeassert,          (True,  Just n))
  go (WaitForAssert w)  (True,  n) = (WaitForAssert (w-1),      (False, Nothing))  -- seen dest_req, wait a little before asserting dest_ack
  go WaitForDeassert    (True,  _) = (WaitForDeassert,          (True,  Nothing))
  go WaitForDeassert    (False, _) = (WaitForAssert maxBound,   (False, Nothing))
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
