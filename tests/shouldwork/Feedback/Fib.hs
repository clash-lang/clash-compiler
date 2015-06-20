module Fib where

import CLaSH.Prelude

fib :: Signal (Unsigned 64)
fib = register 1 fib + register 0 (register 0 fib)

topEntity () = fib

expectedOutput :: Signal (Unsigned 64) -> Signal Bool
expectedOutput = outputVerifier $(v [1 :: Unsigned 64,1,2,3,5])
