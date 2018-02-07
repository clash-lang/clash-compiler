{-# LANGUAGE ExplicitForAll, ScopedTypeVariables #-}
module RecursiveBoxed where

import Clash.Prelude

data B a = B a

topEntity :: B (Int -> Int)
topEntity = case topEntity of B _ -> B (\x -> x)
