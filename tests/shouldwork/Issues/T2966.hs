{-# LANGUAGE CPP #-}

module T2966 where

import Clash.Prelude

data T = A | B { f :: Int}

topEntity :: T -> Int
topEntity = f
{-# CLASH_OPAQUE topEntity #-}
