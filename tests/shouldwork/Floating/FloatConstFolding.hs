module FloatConstFolding where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: Signal System (Float,Double,Signed 8,Signed 9)
topEntity = pure ( resFloat
                 , resDouble
                 , round resFloat
                 , round resDouble
                 )

resFloat  = operations pi_noinline :: Float
resDouble = operations (pi_noinline+1) :: Double

-- this tries to do every operation you can do on a Float or Double
operations :: Floating a => a -> a
operations
-- Num
  = (+ a)
  . (-) (signum b)
  . (* c)
  . abs
  . twiddle
  . negate

-- Floating
  . exp
  . log
  . sqrt
  . (d **)
  . logBase e

  . asin
  . twiddle
  . sin
  . acos
  . twiddle
  . cos
  . atan
  . twiddle
  . tan

  . asinh
  . twiddle
  . sinh
  . acosh
  . twiddle
  . cosh
  . tanh
  . twiddle
  . atanh

-- Fractional
  . (/ f)
  . recip

twiddle :: Floating a => a -> a
twiddle = (+ 1e-5) -- prevent any optimiser from doing: asin . sin <=> id

pi_noinline :: Floating a => a
pi_noinline = pi
{-# NOINLINE pi_noinline #-}

a,b,c,d,e,f :: Floating a => a
a = 1.0
b = a + pi_noinline
c = b + pi_noinline
d = c + pi_noinline
e = d + pi_noinline
f = e + pi_noinline

expectedOutput :: Vec 1 (Float, Double, Signed 8, Signed 9)
expectedOutput = (unpack 0x3F45628A, unpack 0x3FED4161686C9EEE, 1, 1) :> Nil

testBench :: Signal System Bool
testBench = done
  where
    expectOutput = outputVerifier' clk rst expectedOutput
    done         = expectOutput topEntity
    clk          = tbSystemClockGen (not <$> done)
    rst          = systemResetGen
