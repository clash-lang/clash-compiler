{-# OPTIONS_GHC -fno-strictness #-}
module CaseOfErr where

import Clash.Prelude
import qualified Prelude

f :: Bool -> Int
f x = if x then 1 else 0
{-# NOINLINE f #-}

topEntity :: Int
topEntity = f (Prelude.error "QQ")
