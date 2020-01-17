module T1019 where

import Clash.Prelude


f :: SNat m -> Integer
f = snatToInteger
{-# NOINLINE f #-}

topEntity = f (SNat @(LCM 733301111 742))
