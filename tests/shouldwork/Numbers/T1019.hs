{-# LANGUAGE CPP #-}

module T1019 where

import Clash.Prelude


f :: SNat m -> Integer
f = snatToInteger
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE f #-}

topEntity = f (SNat @(LCM 733301111 742))
