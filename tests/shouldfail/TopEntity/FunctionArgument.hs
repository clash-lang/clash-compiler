module FunctionArgument where

import Clash.Prelude

topEntity :: ((Unsigned 8 -> Unsigned 8) -> Unsigned 8) -> Unsigned 8
topEntity p = p (\z -> (z + 1) + z)
