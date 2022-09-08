{-# LANGUAGE OverloadedStrings #-}
module T1945 where

import           Clash.Annotations.Primitive  (Primitive(..), HDL(..), hasBlackBox)
import           Clash.Netlist.Types          (TemplateFunction (..))
import           Clash.Prelude

import           Data.String.Interpolate      (__i)

bb :: a -> a
bb x = x
{-# NOINLINE bb #-}
{-# ANN bb hasBlackBox #-}
{-# ANN bb (InlinePrimitive [VHDL,Verilog,SystemVerilog] [__i|
   [ { "BlackBox" :
        { "name" : "T1945.bb",
          "kind" : "Expression",
          "template": "~ARG[0]",
          "includes" :
            [ { "extension" : "bin"
              , "name" : "mem"
              , "format" : "Haskell"
              , "templateFunction" : "T1945.tf"}

            ]
        }
     }
   ]
   |]) #-}

tf :: TemplateFunction
tf = TemplateFunction used valid (const (pure "QQ"))
 where
  used = [0]
  valid = const False

topEntity :: Bool -> Bool
topEntity = bb
