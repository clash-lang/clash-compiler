module Clash.Cores.LineCoding8b10b.Internal.Encoder where

import Clash.Prelude

import Clash.Cores.LineCoding8b10b.Internal

createMemBlob "table5b6b" Nothing
   [ 0b100111 :: BitVector 6
   , 0b011101
   , 0b101101
   , 0b110001
   , 0b110101
   , 0b101001
   , 0b011001
   , 0b111000
   , 0b111001
   , 0b100101
   , 0b010101
   , 0b110100
   , 0b001101
   , 0b101100
   , 0b011100
   , 0b010111
   , 0b011011
   , 0b100011
   , 0b010011
   , 0b110010
   , 0b001011
   , 0b101010
   , 0b011010
   , 0b111010
   , 0b110011
   , 0b100110
   , 0b010110
   , 0b110110
   , 0b001110
   , 0b101110
   , 0b011110
   , 0b101011
   ]

createMemBlob "table3b4b" Nothing
   [ 0b1011 :: BitVector 4
   , 0b1001
   , 0b0101
   , 0b1100
   , 0b1101
   , 0b1010
   , 0b0110
   , 0b1110
   ]

lookupSubBlock6 ::
  BitVector 5 ->
  BitVector 6
lookupSubBlock6 = asyncRomBlobPow2 table5b6b . unpack

lookupSubBlock4 ::
  BitVector 3 ->
  BitVector 4
lookupSubBlock4 = asyncRomBlobPow2 table3b4b . unpack

encode5b6b ::
  -- | Running disparity
  Bool ->
  -- | Data word
  Symbol8b10b ->
  -- | Tuple containing the new running disparity and the sub-block
  (Bool, BitVector 6)
encode5b6b rd0 sym = (rd1, if inv then complement sb6_1 else sb6_1)
 where
  rd1 =
    case disparity1 of
      SubBlockNeutral -> rd0
      _               -> not rd0

  (inv, disparity1, sb6_1) =
    case (isCw sym, sb5, disparity0) of
      (True, 28, _              ) -> (rd0  , SubBlockPositive, 0b001111)
      (_   , 7 , _              ) -> (rd0  , disparity0      , sb6_0)
      (_   , _ , SubBlockNeutral) -> (False, disparity0      , sb6_0)
      _                           -> (rd0  , disparity0      , sb6_0)

  (_, disparity0) = subBlockDisparity6b sb6_0
  sb6_0 = lookupSubBlock6 sb5
  sb5 = snd $ unpack @(BitVector 3, BitVector 5) $ fromSymbol sym

encode3b4b ::
  -- | Running disparity
  Bool ->
  -- | Data word
  Symbol8b10b ->
  -- | Tuple containing the new running disparity and the sub-block
  (Bool, BitVector 4)
encode3b4b rd0 sym = (rd1, if inv then complement sb4_1 else sb4_1)
 where
  rd1 =
    case disparity of
      SubBlockNeutral -> rd0
      _               -> not rd0

  (inv, sb4_1) =
    case (isCw sym, sb3, disparity) of
      (_   , 3, _              )                        -> (rd0  , sb4_0)
      (_   , 7, _              ) | isCw sym || pickAlt7 -> (rd0  , 0b0111)
      (True, _, SubBlockNeutral)                        -> (not rd0, sb4_0)
      (_   , _, SubBlockNeutral)                        -> (False, sb4_0)
      _                                                 -> (rd0  , sb4_0)

  (_, disparity) = subBlockDisparity4b sb4_0
  sb4_0 = lookupSubBlock4 sb3
  (sb3, sb5) = unpack @(BitVector 3, BitVector 5) $ fromSymbol sym

  pickAlt7 =
    case (rd0, sb5) of
      (False, 17) -> True
      (False, 18) -> True
      (False, 20) -> True
      (True , 11) -> True
      (True , 13) -> True
      (True , 14) -> True
      _           -> False

encode8b10b ::
  -- | Running disparity
  Bool ->
  -- | Data word
  Symbol8b10b ->
  -- | Tuple containing the new running disparity and the code group
  (Bool, BitVector 10)
encode8b10b rd0 sym = (rd2, sb6 ++# sb4)
 where
  (rd1, sb6) = encode5b6b rd0 sym
  (rd2, sb4) = encode3b4b rd1 sym
