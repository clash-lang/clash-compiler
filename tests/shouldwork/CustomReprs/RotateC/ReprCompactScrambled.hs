{-# LANGUAGE CPP #-}

module ReprCompactScrambled
  ( testBench
  , topEntity
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
        'Green
        0b11
        0b01
        []
    , ConstrRepr
        'Blue
        0b11
        0b10
        []
    ]) #-}

{-# ANN module (
  DataReprAnn
    $(liftQ [t| MaybeColor |])
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
    ]) #-}

topEntity :: Top
topEntity = RotateC.topEntity
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = tb topEntity
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBench #-}
