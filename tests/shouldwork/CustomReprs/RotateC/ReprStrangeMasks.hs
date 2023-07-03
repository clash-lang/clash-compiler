{-# LANGUAGE CPP #-}

module ReprStrangeMasks
  ( topEntity
  , testBench
  ) where

import Clash.Prelude
import Clash.Annotations.BitRepresentation

import RotateC (Color(..), MaybeColor(..), Top, tb)
import qualified RotateC

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

topEntity :: Top
topEntity = RotateC.topEntity
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = tb topEntity
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBench #-}
