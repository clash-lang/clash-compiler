{-# LANGUAGE CPP #-}

module DontTranslate where

import Clash.Prelude
import Clash.Annotations.Primitive (dontTranslate)

primitive
  :: Signal System Int
  -> Signal System Int
primitive i =
  (i+5)

{-# OPAQUE primitive #-}
{-# ANN primitive dontTranslate #-}

topEntity = primitive
