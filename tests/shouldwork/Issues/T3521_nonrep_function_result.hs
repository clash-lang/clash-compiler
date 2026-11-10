-- | A recursive function returning a non-representable type can't be inlined
-- away, so Clash should reject it with a proper error instead of crashing on a
-- recursive call graph after normalization.
module T3521_nonrep_function_result where

import Clash.Prelude

data B a = B a

-- The base case keeps GHC from turning g into bottom.
g :: Int -> B (Int -> Int)
g 0 = B id
g n = case g (n - 1) of {B k -> B ((\x -> x + n) . k)}
{-# OPAQUE g #-}

topEntity :: Int -> Int
topEntity i = case g i of {B f -> f i}
