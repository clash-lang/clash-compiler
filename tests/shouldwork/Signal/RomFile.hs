module RomFile where

import Clash.Prelude
import qualified Clash.Explicit.Prelude as Explicit
import Clash.Explicit.Testbench

zeroAt0
  :: HiddenClockReset domain
  => Signal domain (Unsigned 8)
  -> Signal domain (Unsigned 8)
zeroAt0 a = mux en a 0
  where
    en = register False (pure True)

topEntity
  :: Clock System Source
  -> Reset System Asynchronous
  -> Signal System (Unsigned 8)
  -> Signal System (Unsigned 8)
topEntity = exposeClockReset go where
  go rd = zeroAt0 (unpack <$> dout) where
    dout = romFilePow2 "memory.list" rd
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = Explicit.register clk rst 0 (testInput + 1)
    expectedOutput = outputVerifier clk rst $(listToVecTH [0::Unsigned 8,0,1,2,3,4,5,6,7,8])
    done           = expectedOutput (topEntity clk rst testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
