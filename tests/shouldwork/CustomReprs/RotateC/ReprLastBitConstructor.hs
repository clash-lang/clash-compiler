{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TemplateHaskell #-}

module ReprLastBitConstructor
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
    $(liftQ [t| MaybeColor |])
    3
    [ ConstrRepr
        'NothingC
        0b001 -- Mask
        0b000 -- Value
        []
    , ConstrRepr
        'JustC
        0b001   -- Mask
        0b001   -- Value
        [0b110] -- Masks
    ]) #-}

topEntity = RotateC.topEntity
testBench = RotateC.testBench
