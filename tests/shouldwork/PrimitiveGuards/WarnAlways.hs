{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module WarnAlways where

import Clash.Annotations.Primitive (HDL (VHDL), Primitive (InlinePrimitive), warnAlways)
import Clash.Netlist.BlackBox.Types (
  BlackBoxFunction,
  Element (ArgGen, Text),
  emptyBlackBoxMeta,
 )
import Clash.Netlist.Types (BlackBox (BBTemplate))
import Clash.Prelude hiding (Text)

primitiveTF :: BlackBoxFunction
primitiveTF isD primName args ty =
  pure $
    Right (emptyBlackBoxMeta, BBTemplate [Text "5 + ", ArgGen 0 0])

primitive ::
  Signal System Int ->
  Signal System Int
primitive =
  (+ 5)
{-# OPAQUE primitive #-}
{-# ANN primitive (warnAlways "You shouldn't use 'primitive'!") #-}
{-# ANN
  primitive
  ( InlinePrimitive
      [VHDL]
      "[ { \"BlackBoxHaskell\" : { \"name\" : \"WarnAlways.primitive\", \"templateFunction\" : \"WarnAlways.primitiveTF\"}} ]"
  )
  #-}

topEntity = primitive
