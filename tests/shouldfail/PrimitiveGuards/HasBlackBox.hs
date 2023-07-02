{-# LANGUAGE CPP #-}

module HasBlackBox where

import Clash.Prelude
import Clash.Annotations.Primitive (hasBlackBox)

primitive
  :: Signal System Int
  -> Signal System Int
primitive =
  (+5)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE primitive #-}
{-# ANN primitive hasBlackBox #-}

topEntity = primitive
