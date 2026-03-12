-- See ZeroWidth.hs
module FailGracefully3 where

import Clash.Annotations.BitRepresentation
import Clash.Prelude
import Clash.Prelude.Testbench
import Data.Maybe
import GHC.Generics

data Record = Record {myBool :: Bool, myString :: String}
{-# ANN
  module
  ( DataReprAnn $
      (liftQ [t|Record|])
        1
        [ConstrRepr 'Record 0b0 0b0 [0b1, 0b0]]
  )
  #-}

topEntity :: Record -> Record
topEntity (Record b s) = Record (not b) s
