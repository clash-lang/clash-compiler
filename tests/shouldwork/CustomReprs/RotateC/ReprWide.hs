{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TemplateHaskell #-}

module ReprWide
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
    3
    [ ConstrRepr
        'Red
        0b100
        0b100
        []
    , ConstrRepr
        'Blue
        0b010
        0b010
        []
    , ConstrRepr
        'Green
        0b001
        0b001
        []
    ]) #-}

topEntity a = RotateC.topEntity a
testBench = RotateC.testBench
