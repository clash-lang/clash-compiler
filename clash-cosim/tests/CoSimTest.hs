{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}


module Clash.CoSimTest where

------------------------------------------------------
------- IMPORTS ---------------
------------------------------------------------------

import Clash.Explicit.Prelude
import qualified Prelude as P
import qualified Data.List as L
import Data.Int

-- CoSim
import Clash.CoSim

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

-------------------
------- FIR -------
-------------------

dotp
  :: SaturatingNum a
  => Vec (n + 1) a
  -> Vec (n + 1) a
  -> a
dotp as bs = fold boundedPlus (zipWith boundedMult as bs)

fir
  :: ( Default a
     , KnownNat n
     , SaturatingNum a
     )
  => Clock d Source
  -> Reset d Asynchronous
  -> Vec (n + 1) a
  -> Signal d a
  -> Signal d a
fir clk rst coeffs x_t = y_t
  where
    y_t = dotp coeffs <$> bundle xs
    xs = window clk rst x_t

topEntity
  :: Clock System Source
  -> Reset System Asynchronous
  -> Signal System (Signed 64)
  -> Signal System (Signed 64)
topEntity clk rst s = verilog_mult s s


testInput
    :: Clock System Source
    -> Reset System Asynchronous
    -> Signal System (Signed 64)
testInput clk rst = stimuliGenerator clk rst (2:>3:>4:>8:>9:>10:>Nil)


expectedOutput
  :: Signal System (Signed 64)
  -> Signal System Bool
expectedOutput =
    outputVerifier'
        systemClockGen
        systemResetGen
        (4:>12:>1:>20:>Nil)

------------------------------------------------------
------- CO-SIMUlATION --------------------------------
------------------------------------------------------
{-

verilog_fir
  :: t ~ Signed 64
  => Signal d t
  -> Signal d t
verilog_fir x = [verilog|
    reg clk = 1, rst_n = 1;

    initial begin
        #1 rst_n = 0;
        #2 rst_n = 1;
    end

    always begin
        #500 clk = ~clk;
    end

    CoSimTest_topEntity_0 dm(${x}, clk, rst_n, result);

    |] defaultSettings { period = 1000
                       , files = ["./verilog/coSimTest"]
                       }
-}

verilog_mult
  :: t ~ Signed 64
  => Signal d t
  -> Signal d t
  -> Signal d t
verilog_mult x y = [verilog|
  parameter data_width = 64;

  input  signed [0:data_width-1] ${y};
  input  signed [0:data_width-1] ${x};
  output signed [0:data_width-1] result;

  assign result = ${x} * ${y};

  |] --defaultSettings

{-
verilog_reg
  :: t ~ Signed 64
  => Signal d t
  -> Signal d t
verilog_reg x = s
    where
        sx = s + x
        s = [verilog|
          parameter data_width = 64;

          input      [data_width-1:0] ${sx};
          output reg [data_width-1:0] result;
          reg clk = 0, rst = 1;

          initial begin
              #1 rst = 0;
              #2 rst = 1;
          end

          always begin
              #5 clk = ~ clk;
          end

          always @(posedge(clk) or negedge rst) begin
              if (~ rst) begin
                  result <= 0;
              end else begin
                  result <= ${sx};
              end
          end

          |] defaultSettings { period = 10
                             , resetFase = True
                             }-}

main :: IO ()
main =
    putStrLn $ show
             $ sampleN 5
             $ topEntity
                  systemClockGen
                  systemResetGen
                  (testInput systemClockGen systemResetGen)
