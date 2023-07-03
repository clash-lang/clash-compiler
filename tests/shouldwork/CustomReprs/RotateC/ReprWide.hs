{-# LANGUAGE CPP #-}

module ReprWide
  ( topEntity
  , testBench
  ) where

import Clash.Prelude
import Clash.Annotations.BitRepresentation

import RotateC (Color(..), Top, tb)
import qualified RotateC

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

topEntity :: Top
topEntity = RotateC.topEntity
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = tb topEntity
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBench #-}
