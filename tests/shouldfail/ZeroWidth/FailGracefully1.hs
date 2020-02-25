-- See ZeroWidth.hs
module FailGracefully1 where

import Clash.Prelude.Testbench
import Clash.Prelude
import GHC.Generics
import Clash.Annotations.BitRepresentation
import Data.Maybe

data SProduct
  = S Bool String
  | P Bool String Bool
{-# ANN module ( DataReprAnn
                   $(liftQ [t|SProduct|])
                   3
                   [ ConstrRepr 'S 0b100 0b100 [0b001, 0b000]
                   , ConstrRepr 'P 0b100 0b000 [0b010, 0b000, 0b001]
                   ] ) #-}

topEntity :: SProduct -> SProduct
topEntity (S b s) = S (not b) s
topEntity (P b1 s b2) = P (not b1) s (not b2)
