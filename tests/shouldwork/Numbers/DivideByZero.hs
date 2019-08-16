-- Test that the compiler doesn't crash on any DivideByZero exceptions
module DivideByZero where

import Clash.Prelude
import Clash.Explicit.Testbench
import Data.Word
import Data.Int

topEntity = pure @(Signal System)
  (test @(Unsigned 7)
  ,test @(Signed 9)
  ,test @(BitVector 11)
  ,test @(SatIndex 'SatError 13)
  ,test @Int
  ,test @Integer
  ,test @Word16
  ,test @Int32
  )
{-# NOINLINE topEntity #-}

test :: Integral a => (a,a,a,a)
test = (div0,quot0,mod0,rem0)
  where
    div0  = 1 `div`  0
    quot0 = 2 `quot` 0
    mod0  = 3 `mod`  0
    rem0  = 4 `rem`  0

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier clk rst $ (undefined,undefined,undefined,undefined,undefined,undefined,undefined,undefined) :> Nil
    done           = expectedOutput topEntity
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
