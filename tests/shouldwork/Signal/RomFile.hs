module RomFile where

import Clash.Prelude

zeroAt0
  :: HasClockReset domain gated synchronous
  => Signal domain (Unsigned 8)
  -> Signal domain (Unsigned 8)
zeroAt0 a = mux en a 0
  where
    en = register False (pure True)

topEntity
  :: SystemClockReset
  => Signal System (Unsigned 8)
  -> Signal System (Unsigned 8)
topEntity rd = zeroAt0 (unpack <$> dout)
  where
    dout = romFilePow2 "memory.list" rd
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = register 0 (testInput + 1)
    expectedOutput = outputVerifier $(listToVecTH [0::Unsigned 8,0,1,2,3,4,5,6,7,8])
    done           = expectedOutput (topEntity testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
