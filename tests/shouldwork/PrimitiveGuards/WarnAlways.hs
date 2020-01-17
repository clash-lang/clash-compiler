{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}

module WarnAlways where

import Clash.Prelude hiding (Text)
import Clash.Annotations.Primitive (warnAlways)
import Clash.Netlist.Types (BlackBox(BBTemplate))
import Clash.Netlist.BlackBox.Types (BlackBoxFunction, emptyBlackBoxMeta, Element(ArgGen,Text))
import Clash.Annotations.Primitive (Primitive(InlinePrimitive), HDL(VHDL))

primitiveTF :: BlackBoxFunction
primitiveTF isD primName args ty = pure $
  Right ( emptyBlackBoxMeta, BBTemplate [Text "5 + ", ArgGen 0 0])

primitive
  :: Signal System Int
  -> Signal System Int
primitive =
  (+5)

{-# NOINLINE primitive #-}
{-# ANN primitive (warnAlways "You shouldn't use 'primitive'!") #-}
{-# ANN primitive (InlinePrimitive [VHDL] "[ { \"BlackBoxHaskell\" : { \"name\" : \"WarnAlways.primitive\", \"templateFunction\" : \"WarnAlways.primitiveTF\"}} ]") #-}

topEntity = primitive
