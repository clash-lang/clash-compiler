{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}

module ReprCompact
  ( colorRepr
  , maybeColorRepr
  ) where

import Rotate
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

maybeColorRepr :: DataRepr (Maybe Color)
maybeColorRepr =
  DataRepr
    2
    -- How do we represent our constructors?
    [ ConstrRepr
        'Nothing
        0b11 -- Mask
        0b11 -- Value
        []
    , ConstrRepr
        'Just
        0b00   -- Mask
        0b00   -- Value
        [0b11] -- Masks
    ]
