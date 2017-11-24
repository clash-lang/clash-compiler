module Fib where

import Clash.Prelude

fib :: HasClockReset dom gated synchronous => Signal dom (Unsigned 64)
fib = register 1 fib + register 0 (register 0 fib)

topEntity
  :: SystemClockReset
  => Signal System (Unsigned 64)
topEntity = fib
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    expectedOutput = outputVerifier $(listToVecTH [1 :: Unsigned 64,1,2,3,5])
    done           = expectedOutput topEntity
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
