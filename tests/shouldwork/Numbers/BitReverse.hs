module BitReverse where
import Clash.Prelude
import Clash.Explicit.Testbench
import Data.Word

-- ghc-8.10 adds new primitives: bitReverse8#, bitReverse16#, bitReverse32# and bitReverse64#

topEntity :: Word8 -> (Word8, Word16, Word32, Word64)
topEntity x = ( bitReverse8  $ numConvert x
              , bitReverse16 $ numConvert x
              , bitReverse32 $ numConvert x
              , bitReverse64 $ numConvert x
              )

testBench :: Signal System Bool
testBench = done
  where
    testInput =
      stimuliGenerator
        clk
        rst
        ( 0 :>
          0b0001 :>
          0b0010 :>
          0b0011 :>
          0b0100 :>
          0b0101 :>
          0b11010001 :>
          Nil
        )

    expectedOutput =
      outputVerifier'
        clk
        rst
        ( map expand $
          0 :>
          0b10000000 :>
          0b01000000 :>
          0b11000000 :>
          0b00100000 :>
          0b10100000 :>
          0b10001011 :>
          Nil
        )

    done = expectedOutput (topEntity <$> testInput)
    clk  = tbSystemClockGen (not <$> done)
    rst  = systemResetGen

expand :: Word8 -> (Word8,Word16,Word32,Word64)
expand x = (numConvert x, numConvert x `shiftL` 8, numConvert x `shiftL` 24, numConvert x `shiftL` 56)
