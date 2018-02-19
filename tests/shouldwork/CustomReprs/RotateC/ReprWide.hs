{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}

module ReprCompactWide
  ( colorRepr
  ) where

import RotateC
import Clash.Annotations.BitRepresentation
import Data.Maybe

import Prelude (undefined)

colorRepr :: DataRepr Color
colorRepr =
  DataRepr
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
    ]

