{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}

module RotateCScrambledRepr
  ( colorRepr
  , maybeColorRepr
  ) where

import RotateCScrambled
import Clash.Annotations.BitRepresentation
import Data.Maybe

import Prelude (undefined)

colorRepr :: DataRepr Color
colorRepr =
  DataRepr
    2
    [ ConstrRepr
        'Red
        0b11
        0b00
        []
    , ConstrRepr
        'Green
        0b11
        0b01
        []
    , ConstrRepr
        'Blue
        0b11
        0b10
        []
    ]

maybeColorRepr :: DataRepr MaybeColor
maybeColorRepr =
  DataRepr
    2
    [ ConstrRepr
        'NothingC
        0b11 -- Mask
        0b11 -- Value
        []
    , ConstrRepr
        'JustC
        0b00   -- Mask
        0b00   -- Value
        [0b11] -- Masks
    ]


