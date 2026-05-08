module T3023 where

import Clash.Prelude
import Clash.Annotations.Primitive

data MyType = C Int
{-# ANN C dontTranslate #-}

topEntity :: Int -> MyType
topEntity = C
