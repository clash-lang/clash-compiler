module T694 where

import Clash.Prelude
import Clash.Annotations.BitRepresentation

data Color = R | G | B
{-# ANN module (DataReprAnn
                  $(liftQ [t|Color|])
                  2
                  [ ConstrRepr 'R 0b11 0b00 []
                  , ConstrRepr 'G 0b11 0b01 []
                  , ConstrRepr 'B 0b11 0b10 []
                  ]) #-}

topEntity = map Just (R :> G :> B :> Nil)
