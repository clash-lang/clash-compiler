{-# LANGUAGE CPP #-}

module HasBlackBox where

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Prelude

primitive ::
  Signal System Int ->
  Signal System Int
primitive =
  (+ 5)
{-# OPAQUE primitive #-}
{-# ANN primitive hasBlackBox #-}

topEntity = primitive
