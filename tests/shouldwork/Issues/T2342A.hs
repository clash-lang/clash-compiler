{-# LANGUAGE CPP #-}

module T2342A where

import Clash.Prelude
import Data.Proxy

foo :: forall n. KnownNat n => Proxy n -> Float
foo Proxy = natToNum @n
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE foo #-}

topEntity :: Float
topEntity = foo @10 Proxy
