-- | Block RAMs with 'Vec' and 'RTree' payloads. Synthesis tools should
-- implement each of them in a single block RAM, see @blockRamSynth@ in
-- @tests/Main.hs@.
--
-- In SystemVerilog, these payloads are unpacked arrays, which the primitives
-- need to convert before storing them in a memory of packed elements.
--
-- TODO: 'blockRam' is missing, as Vivado implements it in registers when the
-- SystemVerilog backend renders the memory as an array of 'Vec's, unless
-- passed @-fclash-hdlsyn Vivado@. For the same reason, this test does not run
-- for VHDL.
module InferenceVec where

import Clash.Explicit.Prelude

createDomain vSystem{vName="B", vPeriod=hzToPeriod 150e6}

type Addr = Index 1024
type V = Vec 2 (BitVector 16)
type T = RTree 1 (BitVector 16)

blockRamUVec ::
  Clock System ->
  Enable System ->
  Signal System Addr ->
  Signal System (Maybe (Addr, V)) ->
  Signal System V
blockRamUVec clk en = blockRamU clk noReset en NoClearOnReset d1024
{-# OPAQUE blockRamUVec #-}
{-# ANN blockRamUVec (defSyn "blockRamUVec") #-}

blockRamUTree ::
  Clock System ->
  Enable System ->
  Signal System Addr ->
  Signal System (Maybe (Addr, T)) ->
  Signal System T
blockRamUTree clk en = blockRamU clk noReset en NoClearOnReset d1024
{-# OPAQUE blockRamUTree #-}
{-# ANN blockRamUTree (defSyn "blockRamUTree") #-}

blockRam1Vec ::
  Clock System ->
  Enable System ->
  Signal System Addr ->
  Signal System (Maybe (Addr, V)) ->
  Signal System V
blockRam1Vec clk en =
  blockRam1 clk noReset en NoClearOnReset d1024 (0x0123 :> 0x4567 :> Nil)
{-# OPAQUE blockRam1Vec #-}
{-# ANN blockRam1Vec (defSyn "blockRam1Vec") #-}

blockRam1Tree ::
  Clock System ->
  Enable System ->
  Signal System Addr ->
  Signal System (Maybe (Addr, T)) ->
  Signal System T
blockRam1Tree clk en =
  blockRam1 clk noReset en NoClearOnReset d1024 (v2t (0x0123 :> 0x4567 :> Nil))
{-# OPAQUE blockRam1Tree #-}
{-# ANN blockRam1Tree (defSyn "blockRam1Tree") #-}

trueDualPortBlockRamVec ::
  Clock System ->
  Clock B ->
  Signal System (RamOp 1024 V) ->
  Signal B (RamOp 1024 V) ->
  (Signal System V, Signal B V)
trueDualPortBlockRamVec = trueDualPortBlockRam
{-# OPAQUE trueDualPortBlockRamVec #-}
{-# ANN trueDualPortBlockRamVec (defSyn "trueDualPortBlockRamVec") #-}

trueDualPortBlockRamTree ::
  Clock System ->
  Clock B ->
  Signal System (RamOp 1024 T) ->
  Signal B (RamOp 1024 T) ->
  (Signal System T, Signal B T)
trueDualPortBlockRamTree = trueDualPortBlockRam
{-# OPAQUE trueDualPortBlockRamTree #-}
{-# ANN trueDualPortBlockRamTree (defSyn "trueDualPortBlockRamTree") #-}

-- | Synthesis tools treat collisions between the ports differently when both
-- use the same clock
trueDualPortBlockRamSingleClockVec ::
  Clock System ->
  Signal System (RamOp 1024 V) ->
  Signal System (RamOp 1024 V) ->
  (Signal System V, Signal System V)
trueDualPortBlockRamSingleClockVec clk = trueDualPortBlockRam clk clk
{-# OPAQUE trueDualPortBlockRamSingleClockVec #-}
{-# ANN trueDualPortBlockRamSingleClockVec (defSyn "trueDualPortBlockRamSingleClockVec") #-}
