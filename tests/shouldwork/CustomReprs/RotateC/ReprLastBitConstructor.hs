{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}

module ReprLastBitConstructor
  ( maybeColorRepr
  ) where

import RotateC
import Clash.Annotations.BitRepresentation
import Data.Maybe

import Prelude (undefined)

maybeColorRepr :: DataRepr MaybeColor
maybeColorRepr =
  DataRepr
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
    ]


