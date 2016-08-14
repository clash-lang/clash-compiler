{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module CoSimTest where

------------------------------------------------------
------- IMPORTS ---------------
------------------------------------------------------

import CLaSH.Prelude
import CLaSH.Signal.Explicit
import CLaSH.Prelude.Mealy
import CLaSH.Prelude.Testbench
import qualified Prelude as P
import qualified Data.List as L
import Data.Int

-- CoSim
import CoSimCLaSH

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

------------------------------------------------------
------- FIR ------- copied from clash-lang.org -------
------------------------------------------------------

dotp :: SaturatingNum a => Vec (n + 1) a -> Vec (n + 1) a -> a
dotp as bs = fold boundedPlus (zipWith boundedMult as bs)

fir :: (Default a, KnownNat n, SaturatingNum a) => Vec (n + 1) (Signal a) -> Signal a -> Signal a
fir coeffs x_t = y_t
  where
    y_t = dotp coeffs xs
    xs  = window x_t

topEntity :: Signal (Signed 16) -> Signal (Signed 16)
topEntity = fir (2:>3:>(-2):>8:>Nil)

testInput :: Signal (Signed 16)
testInput = stimuliGenerator (2:>3:>(-2):>8:>Nil)

expectedOutput :: Signal (Signed 16) -> Signal Bool
expectedOutput = outputVerifier (4:>12:>1:>20:>Nil)

------------------------------------------------------
------- CO-SIMUlATION --------------------------------
------------------------------------------------------

verilog_fir :: t ~ Signed 16 => Signal t -> Signal t
verilog_fir x = s
    where
        s           = coSim source Icarus "mod1" x
        source      = coSimSeq [verilog| 
                            module mod1 (i,o); 
                            
                            reg clk = 1, rst_n = 1;
                            
                            initial begin 
                                #1 rst_n = 0;
                                #2 rst_n = 1;
                            end
                            
                            always begin
                                #500 clk = ~clk;
                            end 
                            
                            input signed [15:0] i;
                            output signed [15:0] o;
                            
                            CoSimTest_topEntity_0 dm(i, clk, rst_n, o);
                            
                            endmodule|] (1000, False) ["./verilog/CoSimTest"]

verilog_mult :: t ~ Signed 64 => Signal t -> Signal t -> Signal t
verilog_mult        = coSim source Icarus "mod2"
    where
        source      = [verilog| module mod2 (Out, Left, Right);              
                                
                                parameter data_width = 64;
                        
                                input  signed [0:data_width-1] Left;  
                                input  signed [0:data_width-1] Right;                                                       
                                output signed [0:data_width-1] Out;                          
                                                                                                           
                                assign Out = Left * Right;                        
                                                                                  
                                endmodule 
                      |]

verilog_reg :: t ~ Signed 64 => Signal t -> Signal t
verilog_reg x       = s 
    where
        s           = coSim source Icarus "mod3" s'
        s'          = s + x
        source      = coSimSeq [verilog| 
                                module mod3 (d,q);  
                                
                                parameter data_width = 64;
                                                                                                                                                                                        
                                input      [data_width-1:0] d;                           
                                output reg [data_width-1:0] q;   
                                
                                reg clk = 0, rst = 0;
                            
                                initial begin 
                                    #1 rst = 1;
                                end
                            
                                always begin
                                    #5 clk = ~ clk;
                                end                           
                                                                                   
                                always @(posedge(clk) or negedge rst) begin  
                                    if (~ rst) begin                               
                                        q <= 0;    
                                    end else begin
                                        q <= d;
                                    end                                                                    
                                end                                             
                                                                                       
                                endmodule                                       
                      |] (10, True) []
