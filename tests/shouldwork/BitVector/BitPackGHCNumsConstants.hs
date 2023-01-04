module BitPackGHCNums where

import Clash.Prelude
import Clash.Explicit.Testbench
import Data.Int
import Data.Word
import Foreign.C.Types

i :: Int
i = 421

i8 :: Int8
i8 = -123

i16 :: Int16
i16 = 9685

i32 :: Int32
i32 = -74275825

i64 :: Int64
i64 = -4814814284

w :: Word
w = 87558

w8 :: Word8
w8 = 213

w16 :: Word16
w16 = 8585

w32 :: Word32
w32 = 5485

w64 :: Word64
w64 = 12303342142421

cu :: CUShort
cu = 41995
