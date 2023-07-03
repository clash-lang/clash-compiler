{-# LANGUAGE CPP #-}

module T1477 where

import Clash.Prelude
import Data.Proxy

type family QQ a
type instance QQ (Signal dom a) = a

f :: forall a . Proxy a -> QQ a -> QQ a
f Proxy = id
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE f #-}

topEntity = f @(Signal System Bool)
