{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Clash.Prelude
import Clash.Prelude.Testbench
import Clash.CoSim (verilog, verilogWithSettings, period, defaultSettings)

import qualified Data.List as L

import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Inline verilog"
    [ inlineMultiplication
    , pointFree
    , fibonacci
    ]

-- | Convert binary function taking signals, to a binary function taking
-- "normal" Haskell types.
bin
    :: forall t a
     . (t ~ Signal System a, NFDataX a)
    => (t -> t -> t)
    -> a
    -> a
    -> a
bin f x y = L.head $ sample $ f
    (stimuliGenerator (x:>Nil))
    (stimuliGenerator (y:>Nil))


-- | Test equalness of fibonacci sequences generated in verilog and haskell
fibonacci :: TestTree
fibonacci = testGroup "Fibonacci" $
    L.map fibonacci' [0, 10, 100, 1000, 10000]

fibonacci' :: Int -> TestTree
fibonacci' n =
    testCase ("n=" L.++ show n) $ fh @?= fv
        where
            fh = L.take n fibonacci_haskell
            fv = L.take n $ sample $ fibonacci_verilog

fibonacci_haskell :: [Signed 64]
fibonacci_haskell = L.map fst $ L.iterate (\(a,b) -> (b,a+b)) (0,1)

fibonacci_verilog :: Signal System (Signed 64)
fibonacci_verilog = [verilogWithSettings|
    parameter data_width = 64;

    input signed [0:data_width-1] ${fake};
    output signed [0:data_width-1] result;

    reg [0:data_width-1] cur;
    reg [0:data_width-1] next;

    reg clk = 0, rst = 1;

    initial begin
        #1 rst = 0;
        #2 rst = 1;
    end

    always begin
        #1 clk = ~ clk;
    end

    always @(posedge(clk) or negedge rst) begin
        if (~ rst) begin
            cur <= 0;
            next <= 1;
        end else begin
            cur <= next;
            next <= cur + next;
        end
    end

    assign result = cur + ${fake};
    |] defaultSettings { period = 2 }
        where
            -- TODO: Support zero argument verilog functions
            fake = 0 :: Signal System (Signed 64)


-- Test a very simple verilog multiplier. We hardcode some unit tests, then
-- but we also use QuickCheck to generate a number of random numbers
inlineMultiplication :: TestTree
inlineMultiplication = testGroup "Multiplication"
  [ testCase "Small numbers"    $ 3 * 5         @?= (bin mult) 3 5
  , testCase "Zeroes"           $ 0 * 0         @?= (bin mult) 0 0
  , testCase "Big numbers"      $ 5646 * 5465   @?= (bin mult) 5646 5465
  , testCase "Negative numbers" $ (-56) * (-54) @?= (bin mult) (-56) (-54)
  , testCase "Negative number"  $ (56) * (-54)  @?= (bin mult) (56) (-54)
  , QC.testProperty "Haskell / Verilog equalness" $
        \x y -> (x * y) == (bin mult) x y
  ]
        where

            mult :: t ~ Signal System (Signed 64) => t -> t -> t
            mult x y = [verilog|
                parameter data_width = 64;

                input  signed [0:data_width-1] ${y};
                input  signed [0:data_width-1] ${x};
                output signed [0:data_width-1] result;

                assign result = ${x} * ${y};
                |]

-- Test a very simple verilog multiplier. We hardcode some unit tests, then
-- but we also use QuickCheck to generate a number of random numbers
pointFree :: TestTree
pointFree = testGroup "Point-free notation"
  [ testCase "Both"                $ 3 - 5 @?= (bin sub2) 3 5
  , testCase "Both (reversed)"     $ 5 - 3 @?= (bin sub2) 5 3
  , testCase "One"                 $ 5 - 3 @?= (bin sub1) 5 3
  , testCase "One (with settings)" $ 5 - 3 @?= (bin sub1s) 5 3
  ]
        where
            sub1s :: t ~ Signal System (Signed 64) => t -> t -> t
            sub1s x = [verilogWithSettings|
               parameter data_width = 64;

               input  signed [0:data_width-1] ${x};
               input  signed [0:data_width-1] ${0};
               output signed [0:data_width-1] result;

               assign result = ${x} - ${0};
              |] defaultSettings { period = 50 }

            sub1 :: t ~ Signal System (Signed 64) => t -> t -> t
            sub1 x = [verilog|
                parameter data_width = 64;

                input  signed [0:data_width-1] ${x};
                input  signed [0:data_width-1] ${0};
                output signed [0:data_width-1] result;

                assign result = ${x} - ${0};
                |]

            sub2 :: t ~ Signal System (Signed 64) => t -> t -> t
            sub2 = [verilog|
                parameter data_width = 64;

                input  signed [0:data_width-1] ${0};
                input  signed [0:data_width-1] ${1};
                output signed [0:data_width-1] result;

                assign result = ${0} - ${1};
                |]
