-- See ZeroWidth.hs
module FailGracefully2 where

import Clash.Annotations.BitRepresentation
import Clash.Prelude
import Clash.Prelude.Testbench
import Data.Maybe
import GHC.Generics

data Product = Product Bool String
{-# ANN
  module
  ( DataReprAnn $
      (liftQ [t|Product|])
        1
        [ConstrRepr 'Product 0b0 0b0 [0b1, 0b0]]
  )
  #-}

topEntity :: Product -> Product
topEntity (Product b s) = Product (not b) s
