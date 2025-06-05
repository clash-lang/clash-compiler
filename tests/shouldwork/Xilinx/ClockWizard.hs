{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ClockWizard where

import Data.String.Interpolate (__i)

import Clash.Annotations.Primitive
import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Xilinx.ClockGen

createDomain vSystem{vName="DomIn", vPeriod=hzToPeriod 24_000_000}
createDomain vXilinxSystem{vName="DomOut", vPeriod=hzToPeriod 300_000_000}

topEntity ::
  Clock DomIn ->
  DiffClock DomIn ->
  Reset DomIn ->
  Signal DomOut (Index 10, Index 10)
topEntity clkInSE clkInDiff rstIn =
  let f clk rst = register clk rst enableGen 0 . fmap (satSucc SatBound)
      (clkA, rstA) = clockWizard clkInSE rstIn
      (clkB, rstB) = clockWizardDifferential clkInDiff rstIn
      o1 = f clkA rstA o1
      o2 = f clkB rstB o2
  in bundle (o1, o2)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench ::
  Signal DomIn Bool
testBench = done
 where
  (o1, o2) = unbundle $ topEntity clkSE clkDiff rst
  done1 = o1 .== maxBound
  done2 = o2 .== maxBound
  done  = unsafeSynchronizer clockGen clkSE $ fmap endVhdlSim $
            strictAnd <$> done1 <*> done2
  strictAnd !a !b = a && b
  clkSE = tbClockGen (not <$> done)
  clkDiff = clockToDiffClock clkSE
  rst = resetGen
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBench #-}

-- Normally we end VHDL sim by stopping the clocks; usually simulation will
-- notice nothing can ever change anymore and end. The @clockWizard@ simulation
-- models keep running even after we stopped the input clocks. In VHDL-93, the
-- best we can do is throw an assertion. Since our CI greps for assertions of
-- severity @error@ to indicate problems, we can assert severity @failure@ and
-- CI will consider it a success.
--
-- NB: The assertion triggers as soon as it observes @done@ is asserted, which
-- is earlier than we normally end simulation. It might miss an error assertion
-- in the final sample.
endVhdlSim ::
  Bool ->
  Bool
endVhdlSim = id
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE endVhdlSim #-}
{-# ANN endVhdlSim (
  let primName = 'endVhdlSim
  in InlineYamlPrimitive [VHDL] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      template: |-
        -- endVhdlSim begin
        assert (not ~ARG[0]) report "Simulation finished" severity failure;
        ~RESULT <= ~ARG[0];
        -- endVhdlSim end
  |]) #-}
{-# ANN endVhdlSim (
  let primName = 'endVhdlSim
  in InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
    BlackBox:
      name: #{primName}
      kind: Expression
      template: ~ARG[0]
  |]) #-}
