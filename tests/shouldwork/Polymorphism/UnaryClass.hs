{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module UnaryClass where

import Clash.Prelude

class KnownNat n => Alias n
instance KnownNat n => Alias n

increment :: Alias n => Unsigned n -> Unsigned n
increment x = x + 1
{-# OPAQUE increment #-}

topEntity :: Unsigned 8 -> Unsigned 8
topEntity = increment
