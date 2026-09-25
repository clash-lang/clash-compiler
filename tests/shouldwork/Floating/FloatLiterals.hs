module FloatLiterals where

import Clash.Prelude
import Clash.Explicit.Testbench

-- GHC 10.0 can represent negative zero and infinities directly in Core
-- literals. Converting those literals through Rational loses information.
topEntity :: Signal System (BitVector 32, BitVector 64, BitVector 32, BitVector 64)
topEntity = pure
  ( pack (-0.0 :: Float)
  , pack (-0.0 :: Double)
  , pack (1 / 0 :: Float)
  , pack (-1 / 0 :: Double)
  )

testBench :: Signal System Bool
testBench = done
 where
  expected = (0x80000000, 0x8000000000000000, 0x7f800000, 0xfff0000000000000) :> Nil
  done = outputVerifier' clk rst expected topEntity
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen
