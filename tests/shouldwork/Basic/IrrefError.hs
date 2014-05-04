module IrrefError where

topEntity :: Maybe Int -> Int
topEntity ~(Just x) = x
