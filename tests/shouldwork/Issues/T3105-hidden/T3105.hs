{-# LANGUAGE OverloadedStrings #-}

-- | Regression test for https://github.com/clash-lang/clash-compiler/issues/3105.
--
-- This module is compiled into the private sublibrary @clash-testsuite:t3105-hidden@,
-- which Cabal registers as a /hidden/ package. The test explicitly exposes it
-- with @-package-id@. The blackbox template function 'myMultiplyTF' lives in
-- the same hidden package, so Clash's primitive compilation (Hint) must honor
-- the same package flags as the main GHC session to find it.
module T3105 where

import Clash.Prelude hiding (Text)
import Clash.Netlist.Types
import Clash.Netlist.BlackBox.Types
import Clash.Annotations.Primitive (Primitive(..), HDL(..))

myMultiplyTF :: BlackBoxFunction
myMultiplyTF _isD _primName _args _ty = pure $
  Right ( emptyBlackBoxMeta
        , BBTemplate [Text "123456 * ", ArgGen 0 0, Text " * ", ArgGen 0 1]
        )

{-# ANN myMultiply (InlinePrimitive [VHDL, Verilog, SystemVerilog] "[ { \"BlackBoxHaskell\" : { \"name\" : \"T3105.myMultiply\", \"templateFunction\" : \"T3105.myMultiplyTF\"}} ]") #-}
myMultiply
  :: Signal System Int
  -> Signal System Int
  -> Signal System Int
myMultiply a b =
  a * b
{-# OPAQUE myMultiply #-}

topEntity
  :: Signal System Int
  -> Signal System Int
topEntity a = myMultiply a a
