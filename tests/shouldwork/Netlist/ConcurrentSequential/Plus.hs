-- TODO
--
-- This currently uses SimIO to force generation of sequential code. However,
-- this means for the time being the test can only be run in Verilog (as VHDL
-- does not support SimIO).

module Plus where

import Clash.Explicit.Prelude
import Clash.Explicit.SimIO
import Clash.Explicit.Testbench

-- A very simple test for concurrent and sequential HDL having the same
-- behaviour. The entities produce successive powers of 2 until overflow (at
-- which point 0 is returned again).
--
-- What makes this simple? It does not use rely on the use of more complex
-- types or blackboxes, so netlist generation has fewer changes to hit edge
-- cases where generation can fail in subtle ways.

initState :: SimIO (Reg (Unsigned 16))
initState = reg 1

machine :: Reg (Unsigned 16) -> Unsigned 16 -> SimIO (Unsigned 16)
machine rx y = do
  x <- readReg rx
  let next = x + y

  writeReg rx next
  pure next

{-# NOINLINE topEntityConc #-}
{-# ANN topEntityConc (defSyn "topEntityConc") #-}
topEntityConc
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Unsigned 16)
topEntityConc clk rst ena =
  let regIn  = mealy clk rst ena (\x y -> (x + y, x + y)) 1 regOut
      regOut = register clk rst ena 0 regIn
   in regOut

{-# NOINLINE topEntitySeq #-}
{-# ANN topEntitySeq (defSyn "topEntitySeq") #-}
topEntitySeq
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Unsigned 16)
topEntitySeq clk rst ena =
  let regIn  = mealyIO clk machine initState regOut
      regOut = register clk rst ena 0 regIn
   in regOut

testBench
  :: Signal System Bool
testBench = done
 where
  clk = tbClockGen (not <$> done)
  rst = resetGen
  ena = enableGen

  c = topEntityConc clk rst ena
  s = topEntitySeq clk rst ena

  check x y =
    assert clk rst "Inconsistent simulation" x y (x .==. 0)

  done =
    ignoreFor clk rst ena (SNat @2) False (check c s)

