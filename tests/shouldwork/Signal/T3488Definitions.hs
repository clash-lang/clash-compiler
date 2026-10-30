module T3488Definitions where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

type Payload = Vec 2 (BitVector 16)

tdpRam ::
  Clock System ->
  Signal System (RamOp 4 Payload) ->
  Signal System (RamOp 4 Payload) ->
  (Signal System Payload, Signal System Payload)
tdpRam clk = trueDualPortBlockRam clk clk

-- | Operations for port A and B. Neither port writes an address the other port
-- accesses in the same cycle, and no port reads an address that has not been
-- written, so all outputs are defined.
ops :: Vec 9 (RamOp 4 Payload, RamOp 4 Payload)
ops =
     (RamWrite 0 (0x0123 :> 0x4567 :> Nil), RamWrite 1 (0x89AB :> 0xCDEF :> Nil))
  :> (RamWrite 2 (0x1111 :> 0x2222 :> Nil), RamRead 0)
  :> (RamRead 1, RamWrite 3 (0x3333 :> 0x4444 :> Nil))
  :> (RamRead 3, RamRead 2)
  :> (RamNoOp, RamRead 1)
  :> (RamWrite 0 (0xA5A5 :> 0x5A5A :> Nil), RamRead 2)
  :> (RamRead 0, RamNoOp)
  :> (RamRead 0, RamRead 3)
  :> (RamRead 1, RamRead 1)
  :> Nil

-- | Mask the undefined power-up output of the RAM
observe ::
  Clock System ->
  (Signal System Payload, Signal System Payload) ->
  Signal System (Payload, Payload)
observe clk = ignoreFor clk noReset enableGen d1 (repeat 0, repeat 0) . bundle

simOut :: Signal System (Payload, Payload)
simOut = observe clk (tdpRam clk opsA opsB)
 where
  clk = clockGen
  (opsA, opsB) = unbundle (stimuliGenerator clk noReset ops)
