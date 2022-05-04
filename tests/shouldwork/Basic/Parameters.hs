{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Parameters where

import qualified Prelude as P

import Control.Monad.State (State)
import Data.List (isInfixOf)
import Data.Monoid (Ap(getAp))
import Data.Text.Prettyprint.Doc.Extra (Doc)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Clash.Backend (blockDecl)
import Clash.Netlist.Ast.Type (HWType(..), PortDirection(..), typeSize)
import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types
import Clash.Netlist.Util (instPort)

import Clash.Prelude
import Clash.Backend (Backend)
import Clash.Annotations.Primitive (Primitive(..), HDL(..))
import Clash.Explicit.Testbench

import qualified Data.String.Interpolate as I

myAddTF :: TemplateFunction
myAddTF = TemplateFunction used valid myAddTemplate
 where
  used    = [1,2]
  valid _ = True

myAddTemplate
  :: Backend s
  => BlackBoxContext
  -> State s Doc
myAddTemplate bbCtx = do
  let [_, (xExp, xTy, _), (yExp, yTy, _)] = bbInputs bbCtx
      [(resExp, resTy)] = bbResults bbCtx
  blockId <- Id.make "my_add_block"
  myAddInstId <- Id.make "my_add_inst"
  let myAddId = Id.unsafeMake "my_add"
  getAp $ blockDecl blockId
    [ InstDecl Comp Nothing [] myAddId myAddInstId
        [ (instPort "size", Integer, Literal Nothing (NumLit . fromIntegral $ typeSize xTy))
        ]
        (NamedPortMap
          [ (instPort "x", In, xTy, xExp)
          , (instPort "y", In, yTy, yExp)
          , (instPort "result", Out, resTy, resExp)
          ])
    ]

{-# ANN myAdd
  (InlinePrimitive [VHDL] [I.i|
  [ { "BlackBox" :
       { "name" : "Parameters.myAdd"
       , "templateFunction" : "Parameters.myAddTF"
       , "kind": "Declaration"
       , "format": "Haskell"
       , "includes" :
         [{ "name" : "my_vhdl_add"
          , "extension" : "vhdl"
          , "template" :
"library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;

entity my_add is
  generic(size : integer);
  port(x      : in unsigned(size-1 downto 0);
       y      : in unsigned(size-1 downto 0);
       result : out unsigned(size-1 downto 0));
end;
architecture structural of my_add is
begin
  result <= x + y;
end;" }] } } ] |] )
 #-}

{-# ANN myAdd
  (InlinePrimitive [Verilog] [I.i|
  [ { "BlackBox" :
      { "name" : "Parameters.myAdd"
      , "templateFunction" : "Parameters.myAddTF"
       , "kind": "Declaration"
       , "format": "Haskell"
       , "includes" :
        [{ "name" : "my_add"
         , "extension" : "v"
         , "imports" : ["~INCLUDENAME[0].v"]
         , "template" :
"module my_add
  ( input [size-1:0] x
  , input [size-1:0] y
  , output wire [size-1:0] result
  );
  parameter size;

  assign result = x + y;
endmodule"       }] } } ] |] )
 #-}

{-# ANN myAdd
  (InlinePrimitive [SystemVerilog] [I.i|
  [ { "BlackBox" :
      { "name" : "Parameters.myAdd"
      , "templateFunction" : "Parameters.myAddTF"
       , "kind": "Declaration"
       , "format": "Haskell"
      , "includes" :
        [{ "name" : "my_add"
         , "extension" : "v"
         , "imports" : ["~INCLUDENAME[0].v"]
         , "template" :
"module my_add
  ( input logic [size-1:0] x
  , input logic [size-1:0] y
  , output logic [size-1:0] result
  );
  parameter size;

  assign result = x + y;
endmodule"       }] } } ] |] )
 #-}

myAdd
  :: KnownNat n
  => Unsigned n
  -> Unsigned n
  -> Unsigned n
myAdd a b = a + b
{-# NOINLINE myAdd #-}

topEntity
  :: Unsigned 32
  -> Unsigned 32
topEntity a = myAdd a a

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst $(listToVecTH [1,2,3,4,5::Unsigned 32])
    expectedOutput = outputVerifier'   clk rst $(listToVecTH [2,4,6,8,10::Unsigned 32])
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
