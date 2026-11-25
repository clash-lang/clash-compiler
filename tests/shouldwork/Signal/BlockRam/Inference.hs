-- | Block RAMs with a 'BitVector' payload. Synthesis tools should implement
-- each of them in a single block RAM, see @blockRamSynth@ in @tests/Main.hs@.
module Inference where

import Clash.Explicit.Prelude

createDomain vSystem{vName="B", vPeriod=hzToPeriod 150e6}

type Addr = Index 1024
type Payload = BitVector 32

blockRamBV ::
  Clock System ->
  Enable System ->
  Signal System Addr ->
  Signal System (Maybe (Addr, Payload)) ->
  Signal System Payload
blockRamBV clk en = blockRam clk en (replicate d1024 0)
{-# OPAQUE blockRamBV #-}
{-# ANN blockRamBV (defSyn "blockRamBV") #-}

blockRamUBV ::
  Clock System ->
  Enable System ->
  Signal System Addr ->
  Signal System (Maybe (Addr, Payload)) ->
  Signal System Payload
blockRamUBV clk en = blockRamU clk noReset en NoClearOnReset d1024
{-# OPAQUE blockRamUBV #-}
{-# ANN blockRamUBV (defSyn "blockRamUBV") #-}

blockRam1BV ::
  Clock System ->
  Enable System ->
  Signal System Addr ->
  Signal System (Maybe (Addr, Payload)) ->
  Signal System Payload
blockRam1BV clk en = blockRam1 clk noReset en NoClearOnReset d1024 0x01234567
{-# OPAQUE blockRam1BV #-}
{-# ANN blockRam1BV (defSyn "blockRam1BV") #-}

trueDualPortBlockRamBV ::
  Clock System ->
  Clock B ->
  Signal System (RamOp 1024 Payload) ->
  Signal B (RamOp 1024 Payload) ->
  (Signal System Payload, Signal B Payload)
trueDualPortBlockRamBV = trueDualPortBlockRam
{-# OPAQUE trueDualPortBlockRamBV #-}
{-# ANN trueDualPortBlockRamBV (defSyn "trueDualPortBlockRamBV") #-}

-- | Synthesis tools treat collisions between the ports differently when both
-- use the same clock
trueDualPortBlockRamSingleClockBV ::
  Clock System ->
  Signal System (RamOp 1024 Payload) ->
  Signal System (RamOp 1024 Payload) ->
  (Signal System Payload, Signal System Payload)
trueDualPortBlockRamSingleClockBV clk = trueDualPortBlockRam clk clk
{-# OPAQUE trueDualPortBlockRamSingleClockBV #-}
{-# ANN trueDualPortBlockRamSingleClockBV (defSyn "trueDualPortBlockRamSingleClockBV") #-}
