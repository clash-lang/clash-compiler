{-# LANGUAGE CPP #-}

module ReprLastBitConstructor
  ( topEntity
  , testBench
  ) where

import Clash.Prelude
import Clash.Annotations.BitRepresentation

import RotateC (MaybeColor(..), Top, tb)
import qualified RotateC

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

topEntity :: Top
topEntity = RotateC.topEntity
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = tb topEntity
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBench #-}
