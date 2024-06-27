{-# LANGUAGE CPP #-}

-- |
--   Copyright   :  (C) 2024, QBayLogic B.V.
--   License     :  BSD2 (see the file LICENSE)
--   Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
--
--   8b/10b encoding and decoding functions
module Clash.Cores.LineCoding8b10b where

import qualified Clash.Cores.LineCoding8b10b.Decoder as Dec
import qualified Clash.Cores.LineCoding8b10b.Encoder as Enc
import Clash.Prelude

-- | Data type that contains a 'BitVector 8' with the corresponding error
--   condition of the decode function
data DataWord
  = Dw (BitVector 8)
  | Cw (BitVector 8)
  | DwError (BitVector 8)
  | RdError (BitVector 8)
  deriving (Generic, NFDataX, Eq, Show)

-- | Function to check whether a 'DataWord' results in a data word
isDw :: DataWord -> Bool
isDw (Dw _) = True
isDw _ = False

-- | Function to check whether a 'DataWord' results in a data word or
--   control word
isValidDw :: DataWord -> Bool
isValidDw (Cw _) = True
isValidDw dw = isDw dw

-- | Function to convert a 'DataWord' to a plain 'BitVector 8'
fromDw :: DataWord -> BitVector 8
fromDw dw = case dw of
  Dw _dw -> _dw
  Cw _dw -> _dw
  _ -> 0

-- | Take the running disparity and the current code group, and return a tuple
--   containing the new running disparity and a 'DataWord' containing the
--   decoded value. This function uses a 'MemBlob' to store the decoder lookup
--   table.
decode8b10b ::
  -- | Running disparity
  Bool ->
  -- | Code group
  BitVector 10 ->
  -- | Tuple containing the new running disparity and the 'DataWord'
  (Bool, DataWord)
decode8b10b rd cg = (rdNew, dw)
 where
  dw
    | cgEr = DwError (pack _dw)
    | rdEr = RdError (pack _dw)
    | cw = Cw (pack _dw)
    | otherwise = Dw (pack _dw)

  (statusBits, _dw) =
    splitAt d4
      $ bv2v
      $ asyncRomBlobPow2
        $(memBlobTH Nothing Dec.decoderLut)
      $ unpack (bitCoerce rd ++# pack (reverse $ bv2v cg))

  rdEr = bitCoerce $ head statusBits
  cgEr = bitCoerce $ statusBits !! (1 :: Index 4)
  cw = bitCoerce $ statusBits !! (2 :: Index 4)
  rdNew = bitCoerce $ last statusBits

{-# CLASH_OPAQUE decode8b10b #-}

-- | Take the running disparity and the current 'DataWord', and return a tuple
--   containing the new running disparity and a 'BitVector' containing the
--   encoded value. This function uses a 'MemBlob' to store the encoder lookup
--   table.
encode8b10b ::
  -- | Running disparity
  Bool ->
  -- | Data word
  DataWord ->
  -- | Tuple containing the new running disparity and the code group
  (Bool, BitVector 10)
encode8b10b rd dw = out
 where
  cw = case dw of
    Cw _ -> 1
    _ -> 0

  (statusBits, cg) =
    splitAt d2
      $ bv2v
      $ asyncRomBlobPow2
        $(memBlobTH Nothing Enc.encoderLut)
      $ unpack (cw ++# bitCoerce rd ++# fromDw dw)

  rdNew = bitCoerce $ last statusBits

  out = case dw of
    Cw _ -> (rdNew, pack $ reverse cg)
    Dw _ -> (rdNew, pack $ reverse cg)
    _ -> (rd, 0)

{-# CLASH_OPAQUE encode8b10b #-}
