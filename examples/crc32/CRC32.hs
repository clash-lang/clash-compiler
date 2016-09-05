module CRC32 where

import CLaSH.Prelude
import Data.Char
import qualified Data.List as L

import CRC32Table

crc32Step :: BitVector 32 -> BitVector 8 -> BitVector 32
crc32Step prevCRC byte = entry `xor` (prevCRC `shiftR` 8)
  where
    -- We use `$(lift crc32Table)` instead of just `crc32Table` to
    -- force the reduction of `crc32Table` to a vector literal.
    entry = asyncRom $(lift crc32Table) (truncateB prevCRC `xor` byte)

crc32 :: Signal (BitVector 8) -> Signal (BitVector 32)
crc32 = moore crc32Step complement 0xFFFFFFFF . register 0

-- show CRC values as 32-bit unsigned numbers
topEntity :: Signal (BitVector 8) -> Signal (Unsigned 32)
topEntity = fmap unpack . crc32

-- test bench
testInput :: Signal (BitVector 8)
testInput = stimuliGenerator $(listToVecTH (L.map (fromIntegral . ord) "CLaSH" :: [BitVector 8]))

expectedOutput :: Signal (Unsigned 32) -> Signal Bool
expectedOutput = outputVerifier (0 :> 3523407757 :> 2920022741 :> 1535101039 :>
                                 903986498 :> 3095867074 :> 3755410077 :> Nil)
