module NegativeLits where
import Clash.Prelude
import Clash.Explicit.Testbench
import Data.Int
import Data.Word

x :: Num a => a
x = fromInteger (-4)

topEntity :: (Unsigned 8, Word8, Signed 8, Int8)
topEntity = (x,x,x,x)

expectedBv :: BitVector 8
expectedBv = 0b11111100

testBench :: Signal System Bool
testBench = done
  where
    expected = (unpack expectedBv, unpack expectedBv, unpack expectedBv, unpack expectedBv) :> Nil
    expectedOutput = outputVerifier clk rst expected
    done           = expectedOutput (pure topEntity)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
