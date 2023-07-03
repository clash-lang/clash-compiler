{-# LANGUAGE CPP #-}

module CRC32 where

import Clash.Prelude
import Clash.Explicit.Testbench
import Data.Char
import qualified Data.List as L

import CRC32Table

crc32Step :: BitVector 32 -> BitVector 8 -> BitVector 32
crc32Step prevCRC byte = entry `xor` (prevCRC `shiftR` 8)
  where
    -- We use `$(lift crc32Table)` instead of just `crc32Table` to
    -- force the reduction of `crc32Table` to a vector literal.
    entry = asyncRom $(lift crc32Table) (truncateB prevCRC `xor` byte)

crc32
  :: HiddenClockResetEnable tag
  => Signal tag (BitVector 8) -> Signal tag (BitVector 32)
crc32 = moore crc32Step complement 0xFFFFFFFF . register 0

-- show CRC values as 32-bit unsigned numbers
topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (BitVector 8) -> Signal System (Unsigned 32)
topEntity = exposeClockResetEnable (fmap unpack . crc32)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

-- test bench
testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst $(listToVecTH (L.map (fromIntegral . ord) "CLaSH" :: [BitVector 8]))
    expectedOutput = outputVerifier' clk rst (0 :> 3523407757 :> 2920022741 :> 1535101039 :>
                        903986498 :> 3095867074 :> 3755410077 :> Nil)
    done           = expectedOutput (topEntity clk rst enableGen testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
