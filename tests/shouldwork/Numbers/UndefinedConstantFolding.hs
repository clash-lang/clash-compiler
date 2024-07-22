{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}


module UndefinedConstantFolding (topEntity, module ConstantFoldingUtil) where

import Clash.Prelude
import Clash.Sized.Internal.BitVector

import ConstantFoldingUtil


undefBit :: Bit
undefBit = unpack undefined#


-- Test functions bits take an undefined bit as input
-- but produce a defined bit as output
bitTests :: Vec _ (Unsigned 16)
bitTests =
  (if bitToBool (undefBit .&. 0)            then 22000 else     0)   :>
  (if bitToBool (undefBit .|. 1)            then     1 else 22001)   :>
  (if bitToBool ((undefBit `xor` 1) .|. 1)  then     2 else 22002)   :>
  (if bitToBool (shift undefBit 1)          then 22003 else     3)   :>
  (if bitToBool (setBit undefBit 0)         then     4 else 22004)   :>
  (if bitToBool (clearBit undefBit 0)       then 22005 else     5)   :>
  (if bitToBool (shiftL undefBit 1)         then 22006 else     6)   :>
  (if bitToBool (shiftR undefBit 1)         then 22007 else     7)   :>
  Nil


undefBv :: BitVector 5
undefBv = undefined#

-- allIndices =
--   iterate d5 succ 0

bvTests :: Vec _ (Unsigned 16)
bvTests =
  (if bvToBool (undefBv .&. 0)                      then 22100 else     0)   :>
  (if bvToBool (undefBv .|. 0b11111)                then     1 else 22101)   :>
  (if bvToBool ((undefBv `xor` 0b11111).|. 0b11111) then     2 else 22102)   :>
  (if bvToBool (shift undefBv 5)                    then 22103 else     3)   :>

  (if bvToBool (setBit' 0 . setBit' 1 . setBit' 2 . setBit' 3 . setBit' 4 $ undefBv)            then     4 else 22104)   :>
  (if bvToBool (clearBit' 0 . clearBit' 1 . clearBit' 2 . clearBit' 3 . clearBit' 4 $ undefBv)  then 22105 else     5)   :>
  -- (if bvToBool (foldl (setBit) undefBv allIndices)    then     4 else 22104)   :>
  -- (if bvToBool (foldl (clearBit) undefBv allIndices)  then 22105 else     5)   :>

  (if bvToBool (shiftL undefBv 5)                   then 22106 else     6)   :>
  (if bvToBool (shiftR undefBv 5)                   then 22107 else     7)   :>
  (if bvToBool (zeroExtend (truncateB undefBv :: BitVector 0)) then 22108 else     8)   :>
  (if bvToBool (setSlice d4 d0 0 undefBv)                      then 22109 else     9)   :>
  Nil

-- Should we test this in this way?
-- In run in haskell the testBench fails on this
partialDefined :: Vec _ (BitVector 16)
partialDefined =
  (replaceBit 0 undefBit 22200) :>
  Nil

bvToBool :: BitVector 5 -> Bool
bvToBool = (== 0b11111)

setBit' = flip setBit
clearBit' = flip clearBit

topEntity = (bitTests, bvTests, partialDefined)
