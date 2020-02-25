-- See ZeroWidth.hs
module FailGracefully3 where

import Clash.Prelude.Testbench
import Clash.Prelude
import GHC.Generics
import Clash.Annotations.BitRepresentation
import Data.Maybe

data Record = Record { myBool :: Bool, myString :: String }
{-# ANN module ( DataReprAnn
                   $(liftQ [t|Record|])
                   1
                   [ ConstrRepr 'Record 0b0 0b0 [0b1, 0b0] ] ) #-}

topEntity :: Record -> Record
topEntity (Record b s) = Record (not b) s
