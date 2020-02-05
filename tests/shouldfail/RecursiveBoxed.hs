{-# OPTIONS_GHC -fno-strictness  #-}
-- Disable the strictness analyzer
-- Otherwise GHC will replace g with an EmptyCase,
-- removing the recursion that we'd like to test for.

module RecursiveBoxed where

import Clash.Prelude

data B a = B a

g :: B (Int -> Int)
g = case g of {B k -> B ((\x -> x) . k)}

topEntity :: Int -> Int
topEntity i = case g of {B f -> f i}
