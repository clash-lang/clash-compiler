module CRC32Table where

import Clash.Prelude

-- | Table of CRC32 values for all values an 8-bit number can have.
--
-- Must be in a separate file due to TH stage restrictions
crc32Table :: Vec 256 (BitVector 32)
crc32Table = map crcStep $(listToVecTH [0::BitVector 32 .. 255])
  where
    crcStep = last . generate d8 go
    go c    = shiftR c 1 `xor` if c .&. 1 /= 0 then 0xEDB88320 else 0
