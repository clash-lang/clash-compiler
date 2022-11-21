{-# LANGUAGE QuasiQuotes #-}

module Clash.Cores.Xilinx.Synchronizer where

import Clash.Annotations.Primitive
import Clash.Explicit.Prelude

import Data.String.Interpolate      (__i)

-- | See https://docs.xilinx.com/r/en-US/ug901-vivado-synthesis/ASYNC_REG
dualFlipFlopSynchronizer ::
  forall dom1 dom2 a.
  ( KnownDomain dom1
  , KnownDomain dom2
  , NFDataX a
  , BitSize a ~ 1 ) =>
  Clock dom1 ->
  Clock dom2 ->
  Signal dom1 a ->
  Signal dom2 a
dualFlipFlopSynchronizer clk1 clk2 =
    dflipflop clk2
  . dflipflop clk2
  . unsafeSynchronizer clk1 clk2
{-# NOINLINE dualFlipFlopSynchronizer #-}
{-# ANN dualFlipFlopSynchronizer hasBlackBox #-}
{-# ANN dualFlipFlopSynchronizer (InlineYamlPrimitive [Verilog, SystemVerilog] $ [__i|
 BlackBox:
   kind: Declaration
   name: Clash.Cores.Xilinx.Synchronizer.dualFlipFlopSynchronizer
   template: |-
     // begin dualFlipFlopSynchronizer
     (* ASYNC_REG = "TRUE" *) reg ~GENSYM[sync_a][0], ~GENSYM[sync_b][1];
     always @(posedge ~VAR[clk][5]) begin
       ~SYM[1] <= ~SYM[0];
       ~SYM[0] <= ~VAR[in][6];
     end
     assign ~RESULT = ~SYM[1];
     // end dualFlipFlopSynchronizer
|]) #-}
