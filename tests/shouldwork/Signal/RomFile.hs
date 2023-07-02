{-# LANGUAGE CPP #-}

module RomFile where

import Clash.Prelude
import qualified Clash.Explicit.Prelude as Explicit
import Clash.Explicit.Testbench

zeroAt0
  :: HiddenClockResetEnable dom
  => Signal dom (Unsigned 8)
  -> Signal dom (Unsigned 8)
zeroAt0 a = mux en a 0
  where
    en = register False (pure True)

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Unsigned 8)
  -> Signal System (Unsigned 8)
topEntity = exposeClockResetEnable go where
  go rd = zeroAt0 (unpack <$> dout) where
    dout = romFilePow2 "memory.list" rd
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = Explicit.register clk rst enableGen 0 (testInput + 1)
    expectedOutput = outputVerifier' clk rst $(listToVecTH [0::Unsigned 8,0,1,2,3,4,5,6,7,8])
    done           = expectedOutput (topEntity clk rst enableGen testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
