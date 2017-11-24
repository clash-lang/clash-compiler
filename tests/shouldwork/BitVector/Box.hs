module Box where

import Clash.Prelude

topEntity :: BitVector 16 -> (BitVector 16,BitVector 8,Vec 8 Bit)
topEntity vec = (pack  tup
                ,pack  ((0 :: Bit) :> 0 :> 0 :> 0 :> 1 :> 1 :> 1 :> 1 :> Nil)
                ,unpack 0xF0
                )
  where
    tup :: (Vec 8 Bit, Vec 8 Bit)
    tup = unpack vec
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = pure (0x00FF)
    expectedOutput = outputVerifier ((0x00FF,0x0F,1:>1:>1:>1:>0:>0:>0:>0:>Nil):>Nil)
    done           = expectedOutput (topEntity <$> testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
