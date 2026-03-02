{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module BlackBoxFunctionHO where

import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>))
import qualified Prelude as P

import Clash.Annotations.Primitive (HDL (..), Primitive (..))
import Clash.Explicit.Testbench
import Clash.Netlist.BlackBox.Types
import Clash.Netlist.Types
import Clash.Prelude hiding (Text)

myMultiplyTF :: BlackBoxFunction
myMultiplyTF isD primName args ty =
  pure
    $ Right
      ( emptyBlackBoxMeta
      , BBTemplate
          [ Text "resize("
          , Arg 0
          , Text " * "
          , Arg 1
          , Text ",64)"
          ]
      )

{-# ANN
  myMultiply
  ( InlinePrimitive
      [VHDL]
      "[ { \"BlackBoxHaskell\" : { \"name\" : \"BlackBoxFunctionHO.myMultiply\", \"templateFunction\" : \"BlackBoxFunctionHO.myMultiplyTF\"}} ]"
  )
  #-}
myMultiply ::
  Signed 64 ->
  Signed 64 ->
  Signed 64
myMultiply a b =
  a * b
{-# OPAQUE myMultiply #-}

topEntity ::
  Signal System (Signed 64, Vec 3 (Signed 64)) ->
  Signal System (Vec 3 (Signed 64))
topEntity (unbundle -> (scalar, v)) =
  liftA2 map (myMultiply <$> scalar) v

testBench :: Signal System Bool
testBench = done
 where
  done = outputVerifier' clk aclr ((0 :> 3 :> 6 :> Nil) :> Nil) (topEntity inp)
  clk = tbSystemClockGen (not <$> done)
  inp = stimuliGenerator clk aclr ((3, 0 :> 1 :> 2 :> Nil) :> Nil)
  aclr = systemResetGen
