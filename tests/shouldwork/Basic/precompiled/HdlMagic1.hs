{-# OPTIONS_GHC -O2 -fspec-constr #-}
module HdlMagic1 where

import Clash.Prelude
import Clash.Annotations.Primitive

f0 :: Int
f0 = g hdl
 where
  g Nothing = 123
  g (Just VHDL) = 456
  g (Just Verilog) = 789
  g (Just SystemVerilog) = 101112
