-- | A \"multiply-and-accumulate circuit\" that can be compiled
-- to instantly usable HDL (VHDL, or (System)Verilog).
module MAC
  ( circuit
  ) where

import Clash.Prelude

-- | \"multiply and accumulate\", as a pure function.
mac :: Num a => a -> (a, a) -> a
mac acc (x, y) = acc + (x * y)

-- | \"transfer function\" for a mealy machine, represented as a pure
-- function that outputs the new state, as well as the old state.
transfer :: Num a => a -> (a, a) -> (a, a)
transfer acc t = (mac acc t, acc)

-- | A \"mealy machine\" pattern that lifts a pure, combinational
-- transfer function into the world of synchronous sequential logic. The
-- initial accumulator value is 0. This sequential circuit is synchronized
-- to a single incoming/outgoing system clock. The polymorphic number type for
-- the mac/transfer functions is chosen as a 9-bit signed integer.
circuit :: Clock System Source
        -> Reset System Asynchronous
        -> Signal System (Signed 9, Signed 9)
        -> Signal System (Signed 9)
circuit clk rst input = withClockReset clk rst (mealy transfer 0 input)

-- Annotate the circuit with information that describes how it should
-- be synthesized to HDL.
{-# ANN circuit
  (defTop
    { t_name   = "mac"
    , t_inputs = [ PortName "clk"
                 , PortName "rst"
                 , PortField mempty [ PortName "in1", PortName "in2" ]
                 ]
    , t_output = PortName "out"
    }) #-}

