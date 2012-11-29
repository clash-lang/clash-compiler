module DeadRecursiveBoxed where

topEntity :: Bool -> Bool
topEntity y = f ((\x -> x) . (\y -> y)) y

f x y = fst (x, f x y) y
