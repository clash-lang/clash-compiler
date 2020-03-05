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

import GHC.Exts
  ((==#), (<=#), geWord#, isTrue#, minusWord#, plusWord#, uncheckedShiftL#, xor#,
   timesWord2#, quotRemWord2#, and#)
#if MIN_VERSION_base(4,12,0)
import GHC.Exts (addWordC#)
#endif
#if !MIN_VERSION_base(4,12,0)
import GHC.Exts (Int#, Word#, plusWord2#, word2Int#)
#endif
import GHC.Natural (Natural (..))
import GHC.Integer.GMP.Internals
  (BigNat, Integer (..), bigNatToWord, compareBigNat, minusBigNat, minusBigNatWord,
   plusBigNat, plusBigNatWord, sizeofBigNat#, bitBigNat, wordToBigNat2,
   remBigNat, timesBigNat, timesBigNatWord, xorBigNat, wordToBigNat, andBigNat)
#if !MIN_VERSION_base(4,12,0)
import GHC.Integer.GMP.Internals (wordToInteger)
#endif

#include "MachDeps.h"

-- | modular subtraction
subMod :: Natural -> Natural -> Natural -> Natural
subMod (NatS# m#) (NatS# x#) (NatS# y#) =
  if isTrue# (x# `geWord#` y#) then NatS# z# else NatS# (z# `plusWord#` m#)
  where
    z# = x# `minusWord#` y#
subMod NatS#{} _ _ = brokenInvariant
subMod (NatJ# m#) (NatS# x#) (NatS# y#) =
  if isTrue# (x# `geWord#` y#)
    then NatS# (x# `minusWord#` y#)
    else bigNatToNat $ m# `minusBigNatWord` (y# `minusWord#` x#)
subMod (NatJ# m#) (NatS# x#) (NatJ# y#) =
  bigNatToNat $ (m# `minusBigNat` y#) `plusBigNatWord` x#
subMod NatJ#{} (NatJ# x#) (NatS# y#) =
  bigNatToNat $ x# `minusBigNatWord` y#
subMod (NatJ# m#) (NatJ# x#) (NatJ# y#) = case x# `compareBigNat` y# of
  LT -> bigNatToNat $ (m# `minusBigNat` y#) `plusBigNat` x#
  EQ -> NatS# 0##
  GT -> bigNatToNat $ x# `minusBigNat` y#

-- | modular addition
addMod :: Natural -> Natural -> Natural -> Natural
addMod (NatS# m#) (NatS# x#) (NatS# y#) =
  if isTrue# c# || isTrue# (z# `geWord#` m#) then NatS# (z# `minusWord#` m#) else NatS# z#
  where
    !(# z#, c# #) = x# `addWordC#` y#
addMod NatS#{} _ _ = brokenInvariant
addMod (NatJ# m#) (NatS# x#) (NatS# y#) =
  if isTrue# c# then subIfGe (wordToBigNat2 1## z#) m# else NatS# z#
  where
    !(# z#, c# #) = x# `addWordC#` y#
addMod (NatJ# m#) (NatS# x#) (NatJ# y#) = subIfGe (y# `plusBigNatWord` x#) m#
addMod (NatJ# m#) (NatJ# x#) (NatS# y#) = subIfGe (x# `plusBigNatWord` y#) m#
addMod (NatJ# m#) (NatJ# x#) (NatJ# y#) = subIfGe (x# `plusBigNat`     y#) m#

-- | modular multiplication
mulMod :: Natural -> Natural -> Natural -> Natural
mulMod (NatS# m#) (NatS# x#) (NatS# y#) = NatS# r#
  where
    !(# z1#, z2# #) = timesWord2# x# y#
    !(# _, r# #) = quotRemWord2# z1# z2# m#
mulMod NatS#{} _ _ = brokenInvariant
mulMod (NatJ# m#) (NatS# x#) (NatS# y#) =
  bigNatToNat $ wordToBigNat2 z1# z2# `remBigNat` m#
  where
    !(# z1#, z2# #) = timesWord2# x# y#
mulMod (NatJ# m#) (NatS# x#) (NatJ# y#) =
  bigNatToNat $ (y# `timesBigNatWord` x#) `remBigNat` m#
mulMod (NatJ# m#) (NatJ# x#) (NatS# y#) =
  bigNatToNat $ (x# `timesBigNatWord` y#) `remBigNat` m#
mulMod (NatJ# m#) (NatJ# x#) (NatJ# y#) =
  bigNatToNat $ (x# `timesBigNat` y#) `remBigNat` m#

-- | modular multiplication for powers of 2, takes a mask instead of a
-- wrap-around point
mulMod2 :: Natural -> Natural -> Natural -> Natural
mulMod2 (NatS# m#) (NatS# x#) (NatS# y#) = NatS# (z2# `and#` m#)
  where
    !(# _, z2# #) = timesWord2# x# y#
mulMod2 NatS#{} _ _ = brokenInvariant
mulMod2 (NatJ# m#) (NatS# x#) (NatS# y#) =
  bigNatToNat $ wordToBigNat2 z1# z2# `andBigNat` m#
  where
    !(# z1#, z2# #) = timesWord2# x# y#
mulMod2 (NatJ# m#) (NatS# x#) (NatJ# y#) =
  bigNatToNat $ (y# `timesBigNatWord` x#) `andBigNat` m#
mulMod2 (NatJ# m#) (NatJ# x#) (NatS# y#) =
  bigNatToNat $ (x# `timesBigNatWord` y#) `andBigNat` m#
mulMod2 (NatJ# m#) (NatJ# x#) (NatJ# y#) =
  bigNatToNat $ (x# `timesBigNat` y#) `andBigNat` m#

-- | modular negations
negateMod :: Natural -> Natural -> Natural
negateMod _ (NatS# 0##) = NatS# 0##
negateMod (NatS# m#) (NatS# x#) = NatS# (m# `minusWord#` x#)
negateMod NatS#{} _ = brokenInvariant
negateMod (NatJ# m#) (NatS# x#) = bigNatToNat $ m# `minusBigNatWord` x#
negateMod (NatJ# m#) (NatJ# x#) = bigNatToNat $ m# `minusBigNat`     x#

-- | Given a size in bits, return a function that complements the bits in a
-- 'Natural' up to that size.
complementMod
  :: Integer
  -> (Natural -> Natural)
complementMod (S# sz#) =
  if isTrue# (sz# <=# WORD_SIZE_IN_BITS#) then
    let m# = if isTrue# (sz# ==# WORD_SIZE_IN_BITS#) then
#if WORD_SIZE_IN_BITS == 64
                0xFFFFFFFFFFFFFFFF##
#elif WORD_SIZE_IN_BITS == 32
                0xFFFFFFFF##
#else
#error Unhandled value for WORD_SIZE_IN_BITS
#endif
             else
               (1## `uncheckedShiftL#` sz#) `minusWord#` 1##
        go (NatS# x#) = NatS# (x# `xor#` m#)
        go (NatJ# r#) = NatS# (bigNatToWord r# `xor#` m#)
    in  go
  else
    let m# = bitBigNat sz# `minusBigNatWord` 1##

        go (NatS# x#) = bigNatToNat (xorBigNat (wordToBigNat x#) m#)
        go (NatJ# x#) = bigNatToNat (xorBigNat x# m#)
    in  go
complementMod _ = error "size too large"

-- | Keep all the bits up to a certain size
maskMod
  :: Integer
  -> (Natural -> Natural)
maskMod (S# sz#) =
  if isTrue# (sz# <=# WORD_SIZE_IN_BITS#) then
    if isTrue# (sz# ==# WORD_SIZE_IN_BITS#) then
       -- Mask equal to the word size
       let go (NatJ# x#) = NatS# (bigNatToWord x#)
           go n          = n
       in  go
    else
       let m# = (1## `uncheckedShiftL#` sz#) `minusWord#` 1##

           go (NatS# x#) = NatS# (x# `and#` m#)
           go (NatJ# x#) = NatS# (bigNatToWord x# `and#` m#)
       in  go
  else
    let m# = bitBigNat sz#

        -- faster than `andBigNat (m# `minuxBigNatWord` 1##)`
        go (NatJ# x#) = bigNatToNat (remBigNat x# m#)
        -- The mask is larger than the word size, so we can keep all the bits
        go x = x
    in  go
maskMod _ = error "size too large"

bigNatToNat :: BigNat -> Natural
bigNatToNat r# =
  if isTrue# (sizeofBigNat# r# ==# 1#) then
    NatS# (bigNatToWord r#)
  else
    NatJ# r#

subIfGe :: BigNat -> BigNat -> Natural
subIfGe z# m# = case z# `compareBigNat` m# of
  LT -> NatJ# z#
  EQ -> NatS# 0##
  GT -> bigNatToNat $ z# `minusBigNat` m#

#if !MIN_VERSION_base(4,12,0)
addWordC# :: Word# -> Word# -> (# Word#, Int# #)
addWordC# x# y# = (# z#, word2Int# c# #)
  where
    !(# c#, z# #) = x# `plusWord2#` y#

naturalToInteger :: Natural -> Integer
naturalToInteger (NatS# w)  = wordToInteger w
naturalToInteger (NatJ# bn) = Jp# bn
#endif

brokenInvariant :: a
brokenInvariant = error "argument is larger than modulo"
