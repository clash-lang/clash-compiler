module VecFun where

import CLaSH.Prelude

topEntity = work

work :: Vec 3 Int -> Vec 3 Int
work xs = zipWith sel xs funs where
    funs    = fun:>fun:>fun:>Nil
    fun x   = x + 1
    sel x f = f x

testInput :: Signal (Vec 3 Int)
testInput = pure (1:>2:>3:>Nil)

expectedOutput :: Signal (Vec 3 Int) -> Signal Bool
expectedOutput = outputVerifier ((2:>3:>4:>Nil):>Nil)
