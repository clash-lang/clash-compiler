module MultipleGuards where
import Clash.Prelude
import Clash.Annotations.Primitive
import Clash.Util.Interpolate (i)
import Data.String.Interpolate (__i)

test :: Bool
test = True
{-# CLASH_OPAQUE test #-}
{-# ANN test hasBlackBox #-}
{-# ANN test (warnAlways "WARN1") #-}
{-# ANN test (warnAlways "WARN2: You should know that ...") #-}
{-# ANN test (warnAlways "WARN3") #-}
{-# ANN test (InlineYamlPrimitive [VHDL] $  [__i|
  BlackBox:
    name: MultipleGuards.test
    kind: Expression
    template: "true"
 |]) #-}

topEntity :: Bool
topEntity = test
