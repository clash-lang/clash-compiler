{-# LANGUAGE CPP #-}

module HasBlackBox where

import Clash.Prelude
import Clash.Annotations.Primitive (hasBlackBox)

primitive
  :: Signal System Int
  -> Signal System Int
primitive =
  (+5)

{-# OPAQUE primitive #-}
{-# ANN primitive hasBlackBox #-}

topEntity = primitive
