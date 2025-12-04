{-# LANGUAGE CPP #-}

module T2342B where

import Clash.Prelude
import Data.Proxy

foo :: forall n. KnownNat n => Proxy n -> Double
foo Proxy = natToNum @n
{-# OPAQUE foo #-}

topEntity :: Double
topEntity = foo @10 Proxy
