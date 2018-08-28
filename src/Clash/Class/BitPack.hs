{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2016-2017, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK show-extensions #-}

#include "MachDeps.h"

module Clash.Class.BitPack
  ( BitPack (..)
  , bitCoerce
  , boolToBV
  , boolToBit
  , bitToBool
  )
where

import Data.Binary.IEEE754            (doubleToWord, floatToWord, wordToDouble,
                                       wordToFloat)
import Data.Int
import Data.Word
import Foreign.C.Types                (CUShort)
import GHC.TypeLits                   (KnownNat, Nat, type (+))
import Numeric.Half                   (Half (..))
import GHC.Generics
import Prelude                        hiding (map)

import Clash.Class.BitPack.Internal   (deriveBitPackTuples)
import Clash.Class.Resize             (zeroExtend)
import Clash.Sized.BitVector
  (Bit, BitVector, (++#), high, low)
import Clash.Sized.Internal.BitVector
  (BitVector (BV), pack#, split#, checkUnpackUndef, undefError, undefined#,
   unpack#, unsafeToInteger)

{- $setup
>>> :set -XDataKinds
>>> import Clash.Prelude
-}

-- | Convert to and from a 'BitVector'
class BitPack a where
  -- | Number of 'Clash.Sized.BitVector.Bit's needed to represents elements
  -- of type @a@
  --
  -- Can be derived using `GHC.Generics`:
  --
  -- > {-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
  -- >
  -- > import Clash.Prelude
  -- > import GHC.Generics
  -- >
  -- > data MyProductType = MyProductType { a :: Int, b :: Bool }
  -- >   deriving (Generic, BitPack)
  type BitSize a :: Nat
  type BitSize a = GBitSize (Rep a)
  -- | Convert element of type @a@ to a 'BitVector'
  --
  -- >>> pack (-5 :: Signed 6)
  -- 11_1011
  pack   :: a -> BitVector (BitSize a)
  default pack
    :: (Generic a, GBitPack (Rep a), GBitSize (Rep a) ~ BitSize a)
    => a -> BitVector (BitSize a)
  pack = gpack . from
  -- | Convert a 'BitVector' to an element of type @a@
  --
  -- >>> pack (-5 :: Signed 6)
  -- 11_1011
  -- >>> let x = pack (-5 :: Signed 6)
  -- >>> unpack x :: Unsigned 6
  -- 59
  -- >>> pack (59 :: Unsigned 6)
  -- 11_1011
  unpack :: BitVector (BitSize a) -> a
  default unpack
    :: (Generic a, GBitPack (Rep a), GBitSize (Rep a) ~ BitSize a)
    => BitVector (BitSize a) -> a
  unpack = to . gunpack

{-# INLINE bitCoerce #-}
-- | Coerce a value from one type to another through its bit representation.
--
-- >>> pack (-5 :: Signed 6)
-- 11_1011
-- >>> bitCoerce (-5 :: Signed 6) :: Unsigned 6
-- 59
-- >>> pack (59 :: Unsigned 6)
-- 11_1011
bitCoerce :: (BitPack a, BitPack b, BitSize a ~ BitSize b)
          => a
          -> b
bitCoerce = unpack . pack

instance BitPack Bool where
  type BitSize Bool = 1
  pack True  = 1
  pack False = 0

  unpack = checkUnpackUndef $ \bv -> if bv == 1 then True else False

instance BitPack (BitVector n) where
  type BitSize (BitVector n) = n
  pack   v = v
  unpack v = v

instance BitPack Bit where
  type BitSize Bit = 1
  pack   = pack#
  unpack = unpack#

instance BitPack Int where
  type BitSize Int = WORD_SIZE_IN_BITS
  pack   = fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Int8 where
  type BitSize Int8 = 8
  pack   = fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Int16 where
  type BitSize Int16 = 16
  pack   = fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Int32 where
  type BitSize Int32 = 32
  pack   = fromIntegral
  unpack = checkUnpackUndef fromIntegral

#if WORD_SIZE_IN_BITS >= 64
instance BitPack Int64 where
  type BitSize Int64 = 64
  pack   = fromIntegral
  unpack = checkUnpackUndef fromIntegral
#endif

instance BitPack Word where
  type BitSize Word = WORD_SIZE_IN_BITS
  pack   = fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Word8 where
  type BitSize Word8 = 8
  pack   = fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Word16 where
  type BitSize Word16 = 16
  pack   = fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Word32 where
  type BitSize Word32 = 32
  pack   = fromIntegral
  unpack = checkUnpackUndef fromIntegral

#if WORD_SIZE_IN_BITS >= 64
instance BitPack Word64 where
  type BitSize Word64 = 64
  pack   = fromIntegral
  unpack = checkUnpackUndef fromIntegral
#endif

instance BitPack Float where
  type BitSize Float = 32
  pack   = packFloat#
  unpack = checkUnpackUndef unpackFloat#

packFloat# :: Float -> BitVector 32
packFloat# = fromIntegral . floatToWord
{-# NOINLINE packFloat# #-}

unpackFloat# :: BitVector 32 -> Float
unpackFloat# = wordToFloat . fromInteger . unsafeToInteger
{-# NOINLINE unpackFloat# #-}

instance BitPack Double where
  type BitSize Double = 64
  pack   = packDouble#
  unpack = checkUnpackUndef unpackDouble#

packDouble# :: Double -> BitVector 64
packDouble# = fromIntegral . doubleToWord
{-# NOINLINE packDouble# #-}

unpackDouble# :: BitVector 64 -> Double
unpackDouble# = wordToDouble . fromInteger . unsafeToInteger
{-# NOINLINE unpackDouble# #-}

instance BitPack CUShort where
  type BitSize CUShort = 16
  pack   = fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Half where
  type BitSize Half = 16
  pack (Half x) = pack x
  unpack        = checkUnpackUndef $ \x -> Half (unpack x)

instance BitPack () where
  type BitSize () = 0
  pack   _ = minBound
  unpack _ = ()

instance (KnownNat (BitSize b), BitPack a, BitPack b) =>
    BitPack (a,b) where
  type BitSize (a,b) = BitSize a + BitSize b
  pack (a,b) = pack a ++# pack b
  unpack ab  = let (a,b) = split# ab in (unpack a, unpack b)

instance (BitPack a, KnownNat (BitSize a)) => BitPack (Maybe a) where
  type BitSize (Maybe a) = 1 + BitSize a
  pack Nothing  = pack# low ++# undefined#
  pack (Just x) = pack# high ++# pack x
  unpack x = case split# x of
    (c,rest) | checkUnpackUndef unpack# c == low -> Nothing
             | otherwise                         -> Just (unpack rest)

class GBitPack f where
  type GBitSize f :: Nat
  gpack :: f a -> BitVector (GBitSize f)
  gunpack :: BitVector (GBitSize f) -> f a

instance (GBitPack a) => GBitPack (M1 m d a) where
  type GBitSize (M1 m d a) = GBitSize a
  gpack (M1 m1)            = gpack m1
  gunpack b                = M1 (gunpack b)

instance (KnownNat (GBitSize g), GBitPack f, GBitPack g) => GBitPack (f :*: g) where
  type GBitSize (f :*: g) = GBitSize f + GBitSize g
  gpack (m :*: ms)        = gpack m ++# gpack ms
  gunpack b               = gunpack front :*: gunpack back
    where
      (front, back) = split# b

instance (BitPack c) => GBitPack (K1 i c) where
  type GBitSize (K1 i c) = BitSize c
  gpack (K1 i)           = pack i
  gunpack b              = K1 (unpack b)

-- | Zero-extend a 'Bool'ean value to a 'BitVector' of the appropriate size.
--
-- >>> boolToBV True :: BitVector 6
-- 00_0001
-- >>> boolToBV False :: BitVector 6
-- 00_0000
boolToBV :: KnownNat n => Bool -> BitVector (n + 1)
boolToBV = zeroExtend . pack

-- | Convert a Bool to a Bit
boolToBit :: Bool -> Bit
boolToBit = bitCoerce

-- | Convert a Bool to a Bit
bitToBool :: Bit -> Bool
bitToBool = bitCoerce

-- Derive the BitPack instance for tuples of size 3 to 62
deriveBitPackTuples ''BitPack ''BitSize 'pack 'unpack '(++#)
