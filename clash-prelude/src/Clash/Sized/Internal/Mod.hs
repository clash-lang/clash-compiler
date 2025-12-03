{-|
Copyright  :  (C) 2019, Andrew Lelechenko
License    :  MIT
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

This module contains code from: https://hackage.haskell.org/package/mod and has
the following license:

Copyright (c) 2019 Andrew Lelechenko

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute,
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.Sized.Internal.Mod where

import GHC.Exts (eqWord#, leWord#, word2Int#)
import GHC.Exts
  ((<=#), geWord#, isTrue#, minusWord#, plusWord#, uncheckedShiftL#, xor#,
   timesWord2#, quotRemWord2#, and#, addWordC#)
import GHC.Num.BigNat
  (BigNat#, bigNatAdd, bigNatAddWord#, bigNatAnd, bigNatBit#, bigNatCompare,
   bigNatFromWord#, bigNatFromWord2#, bigNatMul, bigNatMulWord#, bigNatRem,
   bigNatSize#, bigNatSubUnsafe, bigNatSubWordUnsafe#, bigNatToWord#, bigNatXor)
import GHC.Num.Natural (Natural (..))

#include "MachDeps.h"

-- | modular subtraction
subMod :: Natural -> Natural -> Natural -> Natural
subMod (NS m#) (NS x#) (NS y#) =
  if isTrue# (x# `geWord#` y#) then NS z# else NS (z# `plusWord#` m#)
  where
    z# = x# `minusWord#` y#
subMod NS{} _ _ = brokenInvariant
subMod (NB m#) (NS x#) (NS y#) =
  if isTrue# (x# `geWord#` y#)
    then NS (x# `minusWord#` y#)
    else bigNatToNat (m# `bigNatSubWordUnsafe#` (y# `minusWord#` x#))
subMod (NB m#) (NS x#) (NB y#) =
  bigNatToNat ((m# `bigNatSubUnsafe` y#) `bigNatAddWord#` x#)
subMod NB{} (NB x#) (NS y#) =
  bigNatToNat (x# `bigNatSubWordUnsafe#` y#)
subMod (NB m#) (NB x#) (NB y#) = case x# `bigNatCompare` y# of
  LT -> bigNatToNat ((m# `bigNatSubUnsafe` y#) `bigNatAdd` x#)
  EQ -> NS 0##
  GT -> bigNatToNat (x# `bigNatSubUnsafe` y#)

-- | modular addition
addMod :: Natural -> Natural -> Natural -> Natural
addMod (NS m#) (NS x#) (NS y#) =
  if isTrue# c# || isTrue# (z# `geWord#` m#) then NS (z# `minusWord#` m#) else NS z#
  where
    !(# z#, c# #) = x# `addWordC#` y#
addMod NS{} _ _ = brokenInvariant
addMod (NB m#) (NS x#) (NS y#) =
  if isTrue# c# then subIfGe (bigNatFromWord2# 1## z#) m# else NS z#
  where
    !(# z#, c# #) = x# `addWordC#` y#
addMod (NB m#) (NS x#) (NB y#) = subIfGe (y# `bigNatAddWord#` x#) m#
addMod (NB m#) (NB x#) (NS y#) = subIfGe (x# `bigNatAddWord#` y#) m#
addMod (NB m#) (NB x#) (NB y#) = subIfGe (x# `bigNatAdd`     y#) m#

-- | modular multiplication
mulMod :: Natural -> Natural -> Natural -> Natural
mulMod (NS m#) (NS x#) (NS y#) = NS r#
  where
    !(# z1#, z2# #) = timesWord2# x# y#
    !(# _, r# #) = quotRemWord2# z1# z2# m#
mulMod NS{} _ _ = brokenInvariant
mulMod (NB m#) (NS x#) (NS y#) =
  bigNatToNat (bigNatFromWord2# z1# z2# `bigNatRem` m#)
  where
    !(# z1#, z2# #) = timesWord2# x# y#
mulMod (NB m#) (NS x#) (NB y#) =
  bigNatToNat ((y# `bigNatMulWord#` x#) `bigNatRem` m#)
mulMod (NB m#) (NB x#) (NS y#) =
  bigNatToNat ((x# `bigNatMulWord#` y#) `bigNatRem` m#)
mulMod (NB m#) (NB x#) (NB y#) =
  bigNatToNat ((x# `bigNatMul` y#) `bigNatRem` m#)

-- | modular multiplication for powers of 2, takes a mask instead of a
-- wrap-around point
mulMod2 :: Natural -> Natural -> Natural -> Natural
mulMod2 (NS m#) (NS x#) (NS y#) = NS (z2# `and#` m#)
  where
    !(# _, z2# #) = timesWord2# x# y#
mulMod2 NS{} _ _ = brokenInvariant
mulMod2 (NB m#) (NS x#) (NS y#) =
  bigNatToNat (bigNatFromWord2# z1# z2# `bigNatAnd` m#)
  where
    !(# z1#, z2# #) = timesWord2# x# y#
mulMod2 (NB m#) (NS x#) (NB y#) =
  bigNatToNat ((y# `bigNatMulWord#` x#) `bigNatAnd` m#)
mulMod2 (NB m#) (NB x#) (NS y#) =
  bigNatToNat ((x# `bigNatMulWord#` y#) `bigNatAnd` m#)
mulMod2 (NB m#) (NB x#) (NB y#) =
  bigNatToNat ((x# `bigNatMul` y#) `bigNatAnd` m#)

-- | modular negations
negateMod :: Natural -> Natural -> Natural
negateMod _ (NS 0##) = NS 0##
negateMod (NS m#) (NS x#) = NS (m# `minusWord#` x#)
negateMod NS{} _ = brokenInvariant
negateMod (NB m#) (NS x#) = bigNatToNat (m# `bigNatSubWordUnsafe#` x#)
negateMod (NB m#) (NB x#) = bigNatToNat (m# `bigNatSubUnsafe`      x#)

-- | Given a size in bits, return a function that complements the bits in a
-- 'Natural' up to that size.
complementMod
  :: Natural
  -> (Natural -> Natural)
complementMod (NS sz#) =
  if isTrue# (sz# `leWord#` WORD_SIZE_IN_BITS##) then
    let m# = if isTrue# (sz# `eqWord#` WORD_SIZE_IN_BITS##) then
#if WORD_SIZE_IN_BITS == 64
                0xFFFFFFFFFFFFFFFF##
#elif WORD_SIZE_IN_BITS == 32
                0xFFFFFFFF##
#else
#error Unhandled value for WORD_SIZE_IN_BITS
#endif
             else
               (1## `uncheckedShiftL#` (word2Int# sz#)) `minusWord#` 1##
        go (NS x#) = NS (x# `xor#` m#)
        go (NB r#) = NS (bigNatToWord# r# `xor#` m#)
    in  go
  else
    let m# = bigNatBit# sz# `bigNatSubWordUnsafe#` 1##

        go (NS x#) = bigNatToNat (bigNatXor (bigNatFromWord# x#) m#)
        go (NB x#) = bigNatToNat (bigNatXor x# m#)
    in  go
complementMod _ = error "size too large"

-- | Keep all the bits up to a certain size
maskMod
  :: Natural
  -> (Natural -> Natural)
maskMod (NS sz#) =
  if isTrue# (sz# `leWord#` WORD_SIZE_IN_BITS##) then
    if isTrue# (sz# `eqWord#` WORD_SIZE_IN_BITS##) then
       -- Mask equal to the word size
       let go (NB x#) = NS (bigNatToWord# x#)
           go n          = n
       in  go
    else
       let m# = (1## `uncheckedShiftL#` (word2Int# sz#)) `minusWord#` 1##

           go (NS x#) = NS (x# `and#` m#)
           go (NB x#) = NS (bigNatToWord# x# `and#` m#)
       in  go
  else
    let m# = bigNatBit# sz#

        -- faster than `bigNatAnd (m# `minuxBigNatWord` 1##)`
        go (NB x#) = bigNatToNat (bigNatRem x# m#)
        -- The mask is larger than the word size, so we can keep all the bits
        go x = x
    in  go
maskMod _ = error "size too large"

bigNatToNat :: BigNat# -> Natural
bigNatToNat r# =
  if isTrue# (bigNatSize# r# <=# 1#) then
    NS (bigNatToWord# r#)
  else
    NB r#

subIfGe :: BigNat# -> BigNat# -> Natural
subIfGe z# m# = case z# `bigNatCompare` m# of
  LT -> NB z#
  EQ -> NS 0##
  GT -> bigNatToNat (z# `bigNatSubUnsafe` m#)

brokenInvariant :: a
brokenInvariant = error "argument is larger than modulo"
