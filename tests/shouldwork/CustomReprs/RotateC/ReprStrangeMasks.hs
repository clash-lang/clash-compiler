{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TemplateHaskell #-}

module ReprStrangeMasks
  ( topEntity
  , testBench
  ) where

import Prelude
import Data.Maybe
import RotateC (Color(..), MaybeColor(..))
import qualified RotateC
import Clash.Annotations.BitRepresentation

{-# ANN module (
  DataReprAnn
    $(liftQ [t| Color |])
    2
    [ ConstrRepr
        'Red
        0b11
        0b00
        []
    , ConstrRepr
        'Blue
        0b11
        0b10
        []
    , ConstrRepr
        'Green
        0b11
        0b01
        []
    ]) #-}

{-# ANN module (
  DataReprAnn
    $(liftQ [t| MaybeColor |])
    5
    [ ConstrRepr
        'NothingC
        0b10101 -- Mask
        0b00000 -- Value
        []
    , ConstrRepr
        'JustC
        0b10101   -- Mask
        0b10101   -- Value
        [0b01010] -- Masks
    ]) #-}


topEntity = RotateC.topEntity
testBench = RotateC.testBench
