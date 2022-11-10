module T2342A where

import Clash.Prelude
import Data.Proxy

foo :: forall n. KnownNat n => Proxy n -> Float
foo Proxy = natToNum @n
{-# NOINLINE foo #-}

topEntity :: Float
topEntity = foo @10 Proxy
