{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module NamedSymbolsAndNumberedSymbols2 where

import           Clash.Annotations.Primitive  (Primitive(..), HDL(..), hasBlackBox)
import           Clash.Netlist.Types          (TemplateFunction (..))
import           Clash.Prelude

import           Data.String.Interpolate      (__i)

bbFirstSym :: a -> a
bbFirstSym x = x
{-# OPAQUE bbFirstSym #-}
{-# ANN bbFirstSym hasBlackBox #-}
{-# ANN bbFirstSym (
  let bbName = show 'bbFirstSym
  in InlineYamlPrimitive [VHDL,Verilog,SystemVerilog] [__i|
    BlackBox:
      name: "#{bbName}"
      kind: Declaration
      template: |-
        ~SYM[mySymbol]
        ~GENSYM[mySymbol][1]
   |]) #-}


topEntity :: Bool -> Bool
topEntity = bbFirstSym
