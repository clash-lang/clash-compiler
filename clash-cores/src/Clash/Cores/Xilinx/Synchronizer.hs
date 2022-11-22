{-|
  Copyright   :  (C) 2022 Google Inc
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Xilinx specific synchronization primitives.
-}

{-# LANGUAGE QuasiQuotes #-}

module Clash.Cores.Xilinx.Synchronizer where

import Clash.Annotations.Primitive
import Clash.Explicit.Prelude

import Data.String.Interpolate (__i)

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
{-# ANN dualFlipFlopSynchronizer (InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
 BlackBox:
   kind: Declaration
   name: Clash.Cores.Xilinx.Synchronizer.dualFlipFlopSynchronizer
   template: |-
     // begin dualFlipFlopSynchronizer
     (* ASYNC_REG = "TRUE" *) reg ~GENSYM[sync_a][0], ~GENSYM[sync_b][1];
     always @(~IF~ACTIVEEDGE[Rising][1]~THENposedge~ELSEnegedge~FI ~ARG[5]) begin
       ~SYM[1] <= ~SYM[0];
       ~SYM[0] <= ~VAR[in][6];
     end
     assign ~RESULT = ~SYM[1];
     // end dualFlipFlopSynchronizer
|]) #-}
{-# ANN dualFlipFlopSynchronizer (InlineYamlPrimitive [VHDL] [__i|
 BlackBox:
   kind: Declaration
   name: Clash.Cores.Xilinx.Synchronizer.dualFlipFlopSynchronizer
   template: |-
     -- begin dualFlipFlopSynchronizer
     ~GENSYM[dualFlipFlopSynchronizer][2] : block
       signal ~GENSYM[sync_a][0] : std_logic;
       signal ~GENSYM[sync_b][1] : std_logic;

       attribute ASYNC_REG : string;
       attribute ASYNC_REG of ~SYM[0] : signal is "TRUE";
       attribute ASYNC_REG of ~SYM[1] : signal is "TRUE";
     begin
       process(~ARG[5])
       begin
         if ~IF~ACTIVEEDGE[Rising][1]~THENrising_edge~ELSEfalling_edge~FI(~ARG[5]) then
           ~SYM[1] <= ~SYM[0];
           ~SYM[0] <= ~VAR[in][6];
         end if;
       end process;

       ~RESULT <= ~SYM[1];
     end block;
     -- end dualFlipFlopSynchronizer
|]) #-}
