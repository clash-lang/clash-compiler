{-# LANGUAGE DataKinds #-}
module ANFNestedLambda where
import Clash.Prelude

-- A function-valued hardware input is unsupported, but normalization must
-- preserve scope so that netlist generation can report that type error.
topEntity :: ((Unsigned 8 -> Unsigned 8) -> Unsigned 8) -> Unsigned 8
topEntity p = p (\z -> (z + 1) + z)
