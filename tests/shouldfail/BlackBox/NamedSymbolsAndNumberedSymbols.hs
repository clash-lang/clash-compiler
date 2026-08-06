{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module NamedSymbolsAndNumberedSymbols where

import           Clash.Annotations.Primitive  (Primitive(..), HDL(..), hasBlackBox)
import           Clash.Netlist.Types          (TemplateFunction (..))
import           Clash.Prelude

import           Data.String.Interpolate      (__i)

bbFirstGensym :: a -> a
bbFirstGensym x = x
{-# OPAQUE bbFirstGensym #-}
{-# ANN bbFirstGensym hasBlackBox #-}
{-# ANN bbFirstGensym (
  let bbName = show 'bbFirstGensym
  in InlineYamlPrimitive [VHDL,Verilog,SystemVerilog] [__i|
    BlackBox:
      name: "#{bbName}"
      kind: Declaration
      template: |-
        ~GENSYM[mySymbol][1]
        ~SYM[mySymbol]
   |]) #-}

topEntity :: Bool -> Bool
topEntity = bbFirstGensym
