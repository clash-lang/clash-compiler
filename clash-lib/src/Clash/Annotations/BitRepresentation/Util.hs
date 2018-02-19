{-# LANGUAGE TemplateHaskell   #-}
module Clash.Annotations.BitRepresentation.Util
  ( bitOrigins
  , bitRanges
  , BitOrigin(..)
  ) where


import Clash.Netlist.Types  (Bit(H,L,U), Size)
import Data.Tuple           (swap)
import Data.List            (findIndex, group, mapAccumL)
import Data.Bits            (testBit, testBit, finiteBitSize)


import Clash.Annotations.BitRepresentation.Internal ( ConstrRepr'(..) )

-- |
data BitOrigin
  = Lit [Bit]
  | Field
      Int
      -- ^ Field number
      Int
      -- ^ Start bit (from..)
      Int
      -- ^ End bit (inclusive, ..downto)
        deriving (Show)

-- | Given a type size and one of its constructor this function will yield a
-- specification of which bits the whole type is made up of. I.e., a
-- construction plan on how to make the whole data structure, given its
-- individual constructor fields.
bitOrigins
  :: Size
  -> ConstrRepr'
  -> [BitOrigin]
bitOrigins size cRepr = mergeOrigins origins
    where
      origins = map (bitOrigin cRepr) (reverse [0..fromIntegral $ size - 1])

      -- | Merge consequtive Constructor and Field fields (if applicable).
      mergeOrigins :: [BitOrigin] -> [BitOrigin]
      mergeOrigins (Lit n : Lit n' : fs) =
        -- Literals can always be merged:
        mergeOrigins $ Lit (n ++ n') : fs
      mergeOrigins (Field n s e : Field n' s' e' : fs)
        -- Consequtive fields with same field number merged:
        | n == n'   = mergeOrigins $ Field n s e' : fs
        -- No merge:
        | otherwise = Field n s e : mergeOrigins (Field n' s' e' : fs)
      -- Base cases:
      mergeOrigins (x:fs) = x : mergeOrigins fs
      mergeOrigins []     = []

      -- | Determine origin of single bit
      bitOrigin :: ConstrRepr' -> Int -> BitOrigin
      bitOrigin (ConstrRepr' _ _ mask value fields) n =
        if testBit mask n then
          Lit [if testBit value n then H else L]
        else
          case findIndex (\fmask -> testBit fmask n) fields of
            Nothing ->
              Lit [U]
            Just fieldn ->
              let fieldbitn = length $ filter id
                                     $ take n
                                     $ map
                                        (testBit $ fields !! fieldn)
                                        [0..finiteBitSize mask - 1] in
              Field fieldn fieldbitn fieldbitn


offsets
  :: Int
  -- ^ Offset
  -> [Bool]
  -- ^ Group
  -> (Int, (Int, [Bool]))
offsets offset group' =
  (length group' + offset, (offset, group'))

-- | Determine consecutively set bits in word. Will produce ranges from high
-- to low. Examples:
--
--   bitRanges 0b10          == [(1,1)]
--   bitRanges 0b101         == [(2,2),(0,0)]
--   bitRanges 0b10011001111 == [(10,10),(7,6),(3,0)]
--
bitRanges :: Word -> [(Int, Int)]
bitRanges word = reverse $ map swap ranges
  where
    ranges  = map (\(ofs, grp) -> (ofs, ofs+length grp-1)) groups'
    groups' = filter (head . snd) groups
    groups  = snd $ mapAccumL offsets 0 (group bits)
    bits    = map (testBit word) [0..finiteBitSize word - 1]
