{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}

module ReprStrangeMasks
  ( colorRepr
  , maybeColorRepr
  ) where

import RotateC
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
        'Blue
        0b11
        0b10
        []
    , ConstrRepr
        'Green
        0b11
        0b01
        []
    ]

maybeColorRepr :: DataRepr MaybeColor
maybeColorRepr =
  DataRepr
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
    ]
