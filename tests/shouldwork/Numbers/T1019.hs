{-# LANGUAGE CPP #-}

module T1019 where

import Clash.Prelude


f :: SNat m -> Integer
f = snatToInteger
{-# OPAQUE f #-}

topEntity = f (SNat @(LCM 733301111 742))
