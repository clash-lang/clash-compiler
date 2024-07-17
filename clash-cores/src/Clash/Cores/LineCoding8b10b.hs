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

-- | Data type that contains a 'BitVector' with the corresponding error
--   condition of the decode function
data Symbol8b10b
  = -- | Correct data word
    Dw (BitVector 8)
  | -- | Correct control word
    Cw (BitVector 8)
  | -- | Incorrect data word
    DwError (BitVector 8)
  | -- | Running disparity error
    RdError (BitVector 8)
  deriving (Generic, NFDataX, Eq, Show)

-- | Function to check whether a 'Symbol8b10b' results in a data word
isDw :: Symbol8b10b -> Bool
isDw (Dw _) = True
isDw _ = False

-- | Function to check whether a 'Symbol8b10b' results in a control word
isCw :: Symbol8b10b -> Bool
isCw (Cw _) = True
isCw _ = False

-- | Function to check whether a 'Symbol8b10b' is not an error value
isValidSymbol :: Symbol8b10b -> Bool
isValidSymbol sym = isDw sym || isCw sym

-- | Function to convert a 'Symbol8b10b' to a plain 'BitVector'
fromSymbol :: Symbol8b10b -> BitVector 8
fromSymbol sym = case sym of
  Dw w -> w
  Cw w -> w
  DwError w -> w
  RdError w -> w

-- | Take the running disparity and the current code group, and return a tuple
--   containing the new running disparity and a 'Symbol8b10b' containing the
--   decoded value. This function uses a 'MemBlob' to store the decoder lookup
--   table.
decode8b10b ::
  -- | Running disparity
  Bool ->
  -- | Code group
  BitVector 10 ->
  -- | Tuple containing the new running disparity and the 'Symbol8b10b'
  (Bool, Symbol8b10b)
decode8b10b rd cg = (rdNew, sym)
 where
  sym
    | cgEr = DwError w
    | rdEr = RdError w
    | cw = Cw w
    | otherwise = Dw w

  (rdEr, cgEr, cw, rdNew, w) =
    unpack
      $ asyncRomBlobPow2
        $(memBlobTH Nothing Dec.decoderLut)
      $ unpack (pack rd ++# cg)

{-# CLASH_OPAQUE decode8b10b #-}

-- | Take the running disparity and the current 'Symbol8b10b', and return a
--   tuple containing the new running disparity and a 'BitVector' containing the
--   encoded value. This function uses a 'MemBlob' to store the encoder lookup
--   table.
encode8b10b ::
  -- | Running disparity
  Bool ->
  -- | Data word
  Symbol8b10b ->
  -- | Tuple containing the new running disparity and the code group
  (Bool, BitVector 10)
encode8b10b rd sym = out
 where
  (_er, rdNew, cg) =
    unpack @(Bool, Bool, BitVector 10)
      $ asyncRomBlobPow2
        $(memBlobTH Nothing Enc.encoderLut)
      $ unpack (pack (isCw sym) ++# pack rd ++# fromSymbol sym)

  out = if isValidSymbol sym then (rdNew, cg) else (rd, 0)

{-# CLASH_OPAQUE encode8b10b #-}
