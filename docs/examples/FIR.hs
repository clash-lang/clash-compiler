module FIR where

import Clash.Prelude

-- The dot product: a summation of products between two vectors
dotp xs ys = sum (zipWith (*) xs ys)

-- The FIR: a dot product of coefficient signals against a sliding window of
-- the input
fir coeffs x = dotp coeffs (window x)

