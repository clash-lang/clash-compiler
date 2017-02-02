{-|
Copyright  :  (C) 2013-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK show-extensions #-}

#include "MachDeps.h"

module CLaSH.Class.BitPack
  ( BitPack (..)
  , bitCoerce
  , boolToBV
  )
where

import Data.Binary.IEEE754            (doubleToWord, floatToWord, wordToDouble,
                                       wordToFloat)
import Data.Int
import Data.Word
import Foreign.C.Types                (CUShort)
import GHC.TypeLits                   (KnownNat, Nat, type (+))
import Numeric.Half                   (Half (..))
import Prelude                        hiding (map)

import CLaSH.Class.Resize             (zeroExtend)
import CLaSH.Sized.BitVector          (BitVector, (++#), high, low)
import CLaSH.Sized.Internal.BitVector (unsafeToInteger, split#)

{- $setup
>>> :set -XDataKinds
>>> import CLaSH.Prelude
-}

-- | Convert to and from a 'BitVector'
class BitPack a where
  -- | Number of 'CLaSH.Sized.BitVector.Bit's needed to represents elements
  -- of type @a@
  type BitSize a :: Nat
  -- | Convert element of type @a@ to a 'BitVector'
  --
  -- >>> pack (-5 :: Signed 6)
  -- 11_1011
  pack   :: a -> BitVector (BitSize a)
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
  pack True  = high
  pack False = low

  unpack bv  = if bv == high then True else False

instance BitPack (BitVector n) where
  type BitSize (BitVector n) = n
  pack   v = v
  unpack v = v

instance BitPack Int where
  type BitSize Int = WORD_SIZE_IN_BITS
  pack   = fromIntegral
  unpack = fromIntegral

instance BitPack Int8 where
  type BitSize Int8 = 8
  pack   = fromIntegral
  unpack = fromIntegral

instance BitPack Int16 where
  type BitSize Int16 = 16
  pack   = fromIntegral
  unpack = fromIntegral

instance BitPack Int32 where
  type BitSize Int32 = 32
  pack   = fromIntegral
  unpack = fromIntegral

#if WORD_SIZE_IN_BITS >= 64
instance BitPack Int64 where
  type BitSize Int64 = 64
  pack   = fromIntegral
  unpack = fromIntegral
#endif

instance BitPack Word where
  type BitSize Word = WORD_SIZE_IN_BITS
  pack   = fromIntegral
  unpack = fromIntegral

instance BitPack Word8 where
  type BitSize Word8 = 8
  pack   = fromIntegral
  unpack = fromIntegral

instance BitPack Word16 where
  type BitSize Word16 = 16
  pack   = fromIntegral
  unpack = fromIntegral

instance BitPack Word32 where
  type BitSize Word32 = 32
  pack   = fromIntegral
  unpack = fromIntegral

#if WORD_SIZE_IN_BITS >= 64
instance BitPack Word64 where
  type BitSize Word64 = 64
  pack   = fromIntegral
  unpack = fromIntegral
#endif

instance BitPack Float where
  type BitSize Float = 32
  pack   = packFloat#
  unpack = unpackFloat#

packFloat# :: Float -> BitVector 32
packFloat# = fromIntegral . floatToWord
{-# NOINLINE packFloat# #-}

unpackFloat# :: BitVector 32 -> Float
unpackFloat# = wordToFloat . fromInteger . unsafeToInteger
{-# NOINLINE unpackFloat# #-}

instance BitPack Double where
  type BitSize Double = 64
  pack   = packDouble#
  unpack = unpackDouble#

packDouble# :: Double -> BitVector 64
packDouble# = fromIntegral . doubleToWord
{-# NOINLINE packDouble# #-}

unpackDouble# :: BitVector 64 -> Double
unpackDouble# = wordToDouble . fromInteger . unsafeToInteger
{-# NOINLINE unpackDouble# #-}

instance BitPack CUShort where
  type BitSize CUShort = 16
  pack   = fromIntegral
  unpack = fromIntegral

instance BitPack Half where
  type BitSize Half = 16
  pack (Half x) = pack x
  unpack x      = Half (unpack x)

instance BitPack () where
  type BitSize () = 0
  pack   _ = minBound
  unpack _ = ()

instance (KnownNat (BitSize b), BitPack a, BitPack b) =>
    BitPack (a,b) where
  type BitSize (a,b) = BitSize a + BitSize b
  pack (a,b) = pack a ++# pack b
  unpack ab  = let (a,b) = split# ab in (unpack a, unpack b)

instance (KnownNat (BitSize c), BitPack (a,b), BitPack c) =>
    BitPack (a,b,c) where
  type BitSize (a,b,c) = BitSize (a,b) + BitSize c
  pack (a,b,c) = pack (a,b) ++# pack c
  unpack (unpack -> ((a,b), c)) = (a,b,c)

instance (KnownNat (BitSize d), BitPack (a,b,c), BitPack d) =>
    BitPack (a,b,c,d) where
  type BitSize (a,b,c,d) = BitSize (a,b,c) + BitSize d
  pack (a,b,c,d) = pack (a,b,c) ++# pack d
  unpack (unpack -> ((a,b,c), d)) = (a,b,c,d)

instance (KnownNat (BitSize e), BitPack (a,b,c,d), BitPack e) =>
    BitPack (a,b,c,d,e) where
  type BitSize (a,b,c,d,e) = BitSize (a,b,c,d) + BitSize e
  pack (a,b,c,d,e) = pack (a,b,c,d) ++# pack e
  unpack (unpack -> ((a,b,c,d), e)) = (a,b,c,d,e)

instance (KnownNat (BitSize f), BitPack (a,b,c,d,e), BitPack f) =>
    BitPack (a,b,c,d,e,f) where
  type BitSize (a,b,c,d,e,f) = BitSize (a,b,c,d,e) + BitSize f
  pack (a,b,c,d,e,f) = pack (a,b,c,d,e) ++# pack f
  unpack (unpack -> ((a,b,c,d,e), f)) = (a,b,c,d,e,f)

instance (KnownNat (BitSize g), BitPack (a,b,c,d,e,f), BitPack g) =>
    BitPack (a,b,c,d,e,f,g) where
  type BitSize (a,b,c,d,e,f,g) = BitSize (a,b,c,d,e,f) + BitSize g
  pack (a,b,c,d,e,f,g) = pack (a,b,c,d,e,f) ++# pack g
  unpack (unpack -> ((a,b,c,d,e,f), g)) = (a,b,c,d,e,f,g)

instance (KnownNat (BitSize h), BitPack (a,b,c,d,e,f,g), BitPack h) =>
    BitPack (a,b,c,d,e,f,g,h) where
  type BitSize (a,b,c,d,e,f,g,h) = BitSize (a,b,c,d,e,f,g) + BitSize h
  pack (a,b,c,d,e,f,g,h) = pack (a,b,c,d,e,f,g) ++# pack h
  unpack (unpack -> ((a,b,c,d,e,f,g), h)) = (a,b,c,d,e,f,g,h)

instance (BitPack a, KnownNat (BitSize a)) => BitPack (Maybe a) where
  type BitSize (Maybe a) = 1 + BitSize a
  pack Nothing  = low  ++# 0
  -- We cannot do `low ++# undefined`, because `BitVector`s underlying
  -- representation is `Integer`, so `low ++# undefined` would make the
  -- entire `BitVector` undefined.
  pack (Just x) = high ++# pack x
  unpack x = case split# x of
    (c,rest) | c == low  -> Nothing
             | otherwise -> Just (unpack rest)

-- | Zero-extend a 'Bool'ean value to a 'BitVector' of the appropriate size.
--
-- >>> boolToBV True :: BitVector 6
-- 00_0001
-- >>> boolToBV False :: BitVector 6
-- 00_0000
boolToBV :: KnownNat n => Bool -> BitVector (n + 1)
boolToBV = zeroExtend . pack
