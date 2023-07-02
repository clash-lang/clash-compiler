{-# LANGUAGE CPP #-}

module Multiply where

import Clash.CoSim
import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

verilog_mult
  :: KnownDomain d
  => Signal d (Signed 64)
  -> Signal d (Signed 64)
  -> Signal d (Signed 64)
verilog_mult x y = [verilog|
  parameter data_width = 64;

  input  signed [0:data_width-1] ${y};
  input  signed [0:data_width-1] ${x};
  output signed [0:data_width-1] result;

  assign result = ${x} * ${y};
  |]
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE verilog_mult #-}

topEntity
  :: Signal System (Signed 64)
  -> Signal System (Signed 64)
topEntity s = verilog_mult s s
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  done = outputVerifier' clk aclr (4:>9:>16:>64:>81:>100:>Nil) res
  res  = topEntity inp
  inp  = stimuliGenerator clk aclr (2:>3:>4:>8:>9:>10:>Nil)
  clk  = tbSystemClockGen (not <$> done)
  aclr = systemResetGen
