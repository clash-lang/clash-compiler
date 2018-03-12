{-# LANGUAGE TemplateHaskell #-}
module Clash.Annotations.BitRepresentation.Util
  ( bitOrigins
  , bitRanges
  , isContinuousMask
  , BitOrigin(..)
  , Bit(..)
  ) where


import Data.Tuple (swap)
import Data.List  (findIndex, group, mapAccumL)
import Data.Bits  (Bits, testBit, testBit, shiftR)

data Bit
  -- | High
  = H
  -- | Low
  | L
  -- | Undefined
  | U
    deriving (Show,Eq)

-- | Result of various utilty functions. Indicates the origin of a certain bit:
-- either a literal from the constructor (or an undefined bit), or from a
-- literal.
data BitOrigin
  -- | Literal (high, low, undefind)
  = Lit [Bit]
  -- | Bits originate from a field. Field /fieldnr/ /from/ /downto/.
  | Field
      Int
      -- Field number
      Int
      -- Start bit (from..)
      Int
      -- End bit (inclusive, ..downto)
        deriving (Show)

-- | Given a type size and one of its constructor this function will yield a
-- specification of which bits the whole type is made up of. I.e., a
-- construction plan on how to make the whole data structure, given its
-- individual constructor fields.
bitOrigins
  :: Int
  -> (Integer, Integer, [Integer])
  -- ^ (mask, value, fields) from CustomRepr or CustomRepr'
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
      bitOrigin :: (Integer, Integer, [Integer]) -> Int -> BitOrigin
      bitOrigin (mask, value, fields) n =
        if testBit mask n then
          Lit [if testBit value n then H else L]
        else
          case findIndex (\fmask -> testBit fmask n) fields of
            Nothing ->
              Lit [U]
            Just fieldn ->
              let fieldbitn = length $ filter id
                                     $ take n
                                     $ bitsToBools (fields !! fieldn) in
              Field fieldn fieldbitn fieldbitn

-- | Convert a number to a list of its bits
-- Output is ordered from least to most significant bit.
-- Only outputs bits until the highest set bit.
--
-- >>> map bitsToBools [0..2]
-- [[],[True],[False,True]])
--
-- This also works for variable sized number like Integer.
-- But not for negative numbers, because negative Integers have infinite bits set.
bitsToBools :: (Num a, Bits a, Ord a) => a -> [Bool]
bitsToBools 0 = []
bitsToBools n | n < 0 = error "Can't deal with negative bitmasks/values"
              | otherwise = testBit n 0 : bitsToBools (n `shiftR` 1)


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
bitRanges :: Integer -> [(Int, Int)]
bitRanges word = reverse $ map swap ranges
  where
    ranges  = map (\(ofs, grp) -> (ofs, ofs+length grp-1)) groups'
    groups' = filter (head . snd) groups
    groups  = snd $ mapAccumL offsets 0 (group bits)
    bits    = bitsToBools word

isContinuousMask :: Integer -> Bool
isContinuousMask word =
  -- Use case expression so we avoid calculating all groups
  case bitRanges word of
    -- At least two groups:
    (_:_:_) -> False
    -- Zero or one group:
    _       -> True
