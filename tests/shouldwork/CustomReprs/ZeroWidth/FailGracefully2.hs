-- See ZeroWidth.hs
module FailGracefully2 where

import Clash.Prelude.Testbench
import Clash.Prelude
import GHC.Generics
import Clash.Annotations.BitRepresentation
import Data.Maybe

data Product = Product Bool String
{-# ANN module ( DataReprAnn
                   $(liftQ [t|Product|])
                   1
                   [ ConstrRepr 'Product 0b0 0b0 [0b1, 0b0] ] ) #-}

topEntity :: Product -> Product
topEntity (Product b s) = Product (not b) s
