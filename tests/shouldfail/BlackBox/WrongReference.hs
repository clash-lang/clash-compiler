{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module WrongReference where

import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>))
import qualified Prelude as P

import Clash.Annotations.Primitive (HDL (..), Primitive (..))
import Clash.Netlist.BlackBox.Types
import Clash.Netlist.Types
import Clash.Prelude hiding (Text)

{-# ANN
  myMultiply
  ( InlinePrimitive
      [VHDL]
      "[ { \"BlackBoxHaskell\" : { \"name\" : \"WrongReference.myMultiplyX\", \"templateFunction\" : \"Foo.bar\"}} ]"
  )
  #-}
myMultiply ::
  Signal System Int ->
  Signal System Int ->
  Signal System Int
myMultiply a b =
  a * b
{-# OPAQUE myMultiply #-}

topEntity ::
  (HiddenClockResetEnable System) =>
  Signal System Int ->
  Signal System Int
topEntity a = myMultiply a a
