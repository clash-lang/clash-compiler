{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module WrongReference where

import qualified Prelude as P
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Clash.Prelude hiding (Text)
import Clash.Netlist.Types
import Clash.Netlist.BlackBox.Types
import Clash.Annotations.Primitive (Primitive(..), HDL(..))

{-# ANN myMultiply (InlinePrimitive [VHDL] "[ { \"BlackBoxHaskell\" : { \"name\" : \"WrongReference.myMultiplyX\", \"templateFunction\" : \"Foo.bar\"}} ]") #-}
myMultiply
  :: Signal System Int
  -> Signal System Int
  -> Signal System Int
myMultiply a b =
  a * b
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE myMultiply #-}

topEntity
  :: HiddenClockResetEnable System
  => Signal System Int
  -> Signal System Int
topEntity a = myMultiply a a
