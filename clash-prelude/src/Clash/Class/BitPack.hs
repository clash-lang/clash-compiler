{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2016-2017, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK show-extensions #-}

#include "MachDeps.h"

module Clash.Class.BitPack
  ( BitPack (..)
  , bitCoerce
  , bitCoerceMap
  , boolToBV
  , boolToBit
  , bitToBool
  , packXWith

  -- * Internals
  , GBitPack(..)
  )
where

import Control.Exception              (catch, evaluate)
import Data.Binary.IEEE754            (doubleToWord, floatToWord, wordToDouble,
                                       wordToFloat)

#if MIN_VERSION_base(4,12,0)
import Data.Complex                   (Complex)
import Data.Ord                       (Down)
#endif

import Data.Int
import Data.Word
import Foreign.C.Types                (CUShort)
import GHC.TypeLits                   (KnownNat, Nat, type (+), type (-))
import Numeric.Half                   (Half (..))
import GHC.Generics
import GHC.TypeLits.Extra             (CLog, Max)
import Prelude                        hiding (map)
import System.IO.Unsafe               (unsafeDupablePerformIO)

import Clash.Promoted.Nat             (SNat(..), snatToNum)
import Clash.Class.BitPack.Internal   (deriveBitPackTuples)
import Clash.Class.Resize             (zeroExtend, resize)
import Clash.Sized.BitVector          (Bit, BitVector, (++#))
import Clash.Sized.Internal.BitVector
  (pack#, split#, checkUnpackUndef, undefined#, unpack#, unsafeToNatural)
import Clash.XException

{- $setup
>>> :set -XDataKinds
>>> import Clash.Prelude
-}

-- | Convert to and from a 'BitVector'
class KnownNat (BitSize a) => BitPack a where
  -- | Number of 'Clash.Sized.BitVector.Bit's needed to represents elements
  -- of type @a@
  --
  -- Can be derived using `GHC.Generics`:
  --
  -- > import Clash.Prelude
  -- > import GHC.Generics
  -- >
  -- > data MyProductType = MyProductType { a :: Int, b :: Bool }
  -- >   deriving (Generic, BitPack)
  type BitSize a :: Nat
  type BitSize a = (CLog 2 (GConstructorCount (Rep a))) + (GFieldSize (Rep a))
  -- | Convert element of type @a@ to a 'BitVector'
  --
  -- >>> pack (-5 :: Signed 6)
  -- 11_1011
  pack   :: a -> BitVector (BitSize a)
  default pack
    :: ( Generic a
       , GBitPack (Rep a)
       , KnownNat (BitSize a)
       , KnownNat constrSize
       , KnownNat fieldSize
       , constrSize ~ CLog 2 (GConstructorCount (Rep a))
       , fieldSize ~ GFieldSize (Rep a)
       , (constrSize + fieldSize) ~ BitSize a
       )
    => a -> BitVector (BitSize a)
  pack = packXWith go
   where
    go a = resize (pack sc) ++# packedFields
     where
      (sc, packedFields) = gPackFields 0 (from a)

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
    :: ( Generic a
       , GBitPack (Rep a)
       , KnownNat constrSize
       , KnownNat fieldSize
       , constrSize ~ CLog 2 (GConstructorCount (Rep a))
       , fieldSize ~ GFieldSize (Rep a)
       , (constrSize + fieldSize) ~ BitSize a
       )
    => BitVector (BitSize a) -> a
  unpack b =
    to (gUnpack sc 0 bFields)
   where
    (checkUnpackUndef unpack . resize -> sc, bFields) = split# b

packXWith
  :: KnownNat n
  => (a -> BitVector n)
  -> a
  -> BitVector n
packXWith f x =
  unsafeDupablePerformIO (catch (f <$> evaluate x)
                                (\(XException _) -> return undefined#))
{-# NOINLINE packXWith #-}

{-# INLINE[1] bitCoerce #-}
-- | Coerce a value from one type to another through its bit representation.
--
-- >>> pack (-5 :: Signed 6)
-- 11_1011
-- >>> bitCoerce (-5 :: Signed 6) :: Unsigned 6
-- 59
-- >>> pack (59 :: Unsigned 6)
-- 11_1011
bitCoerce
  :: (BitPack a, BitPack b, BitSize a ~ BitSize b)
  => a
  -> b
bitCoerce = unpack . pack

-- | Map a value by first coercing to another type through its bit representation.
--
-- >>> pack (-5 :: Signed 32)
-- 1111_1111_1111_1111_1111_1111_1111_1011
-- >>> bitCoerceMap @(Vec 4 (BitVector 8)) (replace 1 0) (-5 :: Signed 32)
-- -16711685
-- >>> pack (-16711685 :: Signed 32)
-- 1111_1111_0000_0000_1111_1111_1111_1011
bitCoerceMap
  :: forall a b . (BitPack a, BitPack b, BitSize a ~ BitSize b)
  => (a -> a)
  -> b
  -> b
bitCoerceMap f = bitCoerce . f . bitCoerce

instance BitPack Bool where
  type BitSize Bool = 1
  pack   = let go b = if b then 1 else 0 in packXWith go
  unpack = checkUnpackUndef $ \bv -> if bv == 1 then True else False

instance KnownNat n => BitPack (BitVector n) where
  type BitSize (BitVector n) = n
  pack     = packXWith id
  unpack v = v

instance BitPack Bit where
  type BitSize Bit = 1
  pack   = packXWith pack#
  unpack = unpack#

instance BitPack Int where
  type BitSize Int = WORD_SIZE_IN_BITS
  pack   = packXWith fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Int8 where
  type BitSize Int8 = 8
  pack   = packXWith fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Int16 where
  type BitSize Int16 = 16
  pack   = packXWith fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Int32 where
  type BitSize Int32 = 32
  pack   = packXWith fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Int64 where
  type BitSize Int64 = 64
  pack   = packXWith fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Word where
  type BitSize Word = WORD_SIZE_IN_BITS
  pack   = packXWith fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Word8 where
  type BitSize Word8 = 8
  pack   = packXWith fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Word16 where
  type BitSize Word16 = 16
  pack   = packXWith fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Word32 where
  type BitSize Word32 = 32
  pack   = packXWith fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Word64 where
  type BitSize Word64 = 64
  pack   = packXWith fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Float where
  type BitSize Float = 32
  pack   = packXWith packFloat#
  unpack = checkUnpackUndef unpackFloat#

packFloat# :: Float -> BitVector 32
packFloat# = fromIntegral . floatToWord
{-# NOINLINE packFloat# #-}

unpackFloat# :: BitVector 32 -> Float
unpackFloat# (unsafeToNatural -> w) = wordToFloat (fromIntegral w)
{-# NOINLINE unpackFloat# #-}

instance BitPack Double where
  type BitSize Double = 64
  pack   = packXWith packDouble#
  unpack = checkUnpackUndef unpackDouble#

packDouble# :: Double -> BitVector 64
packDouble# = fromIntegral . doubleToWord
{-# NOINLINE packDouble# #-}

unpackDouble# :: BitVector 64 -> Double
unpackDouble# (unsafeToNatural -> w) = wordToDouble (fromIntegral w)
{-# NOINLINE unpackDouble# #-}

instance BitPack CUShort where
  type BitSize CUShort = 16
  pack   = packXWith fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Half where
  type BitSize Half = 16
  pack (Half x) = pack x
  unpack        = checkUnpackUndef $ \x -> Half (unpack x)

instance BitPack () where
  type BitSize () = 0
  pack   _ = minBound
  unpack _ = ()

instance (BitPack a, BitPack b) =>
    BitPack (a,b) where
  type BitSize (a,b) = BitSize a + BitSize b
  pack = let go (a,b) = pack a ++# pack b in packXWith go
  unpack ab  = let (a,b) = split# ab in (unpack a, unpack b)

class GBitPack f where
  -- | Size of fields. If multiple constructors exist, this is the maximum of
  -- the sum of each of the constructors fields.
  type GFieldSize f :: Nat

  -- | Number of constructors this type has. Indirectly indicates how many bits
  -- are needed to represent the constructor.
  type GConstructorCount f :: Nat

  -- | Pack fields of a type. Caller should pack and prepend the constructor bits.
  gPackFields
    :: Int
    -- ^ Current constructor
    -> f a
    -- ^ Data to pack
    -> (Int, BitVector (GFieldSize f))
    -- ^ (Constructor number, Packed fields)

  -- | Unpack whole type.
  gUnpack
    :: Int
    -- ^ Construct with constructor /n/
    -> Int
    -- ^ Current constructor
    -> BitVector (GFieldSize f)
    -- ^ BitVector containing fields
    -> f a
    -- ^ Unpacked result

instance GBitPack a => GBitPack (M1 m d a) where
  type GFieldSize (M1 m d a) = GFieldSize a
  type GConstructorCount (M1 m d a) = GConstructorCount a

  gPackFields cc (M1 m1) = gPackFields cc m1
  gUnpack c cc b = M1 (gUnpack c cc b)

instance ( KnownNat (GFieldSize g)
         , KnownNat (GFieldSize f)
         , KnownNat (GConstructorCount f)
         , GBitPack f
         , GBitPack g
         ) => GBitPack (f :+: g) where
  type GFieldSize (f :+: g) = Max (GFieldSize f) (GFieldSize g)
  type GConstructorCount (f :+: g) = GConstructorCount f + GConstructorCount g

  gPackFields cc (L1 l) =
    let (sc, packed) = gPackFields cc l in
    let padding = undefined# :: BitVector (Max (GFieldSize f) (GFieldSize g) - GFieldSize f) in
    (sc, packed ++# padding)
  gPackFields cc (R1 r) =
    let cLeft = snatToNum (SNat @(GConstructorCount f)) in
    let (sc, packed) = gPackFields (cc + cLeft) r in
    let padding = undefined# :: BitVector (Max (GFieldSize f) (GFieldSize g) - GFieldSize g) in
    (sc, packed ++# padding)

  gUnpack c cc b =
    let cLeft = snatToNum (SNat @(GConstructorCount f)) in
    if c < cc + cLeft then
      L1 (gUnpack c cc f)
    else
      R1 (gUnpack c (cc + cLeft) g)

   where
    -- It's a thing of beauty, if I may say so myself!
    (f, _ :: BitVector (Max (GFieldSize f) (GFieldSize g) - GFieldSize f)) = split# b
    (g, _ :: BitVector (Max (GFieldSize f) (GFieldSize g) - GFieldSize g)) = split# b


instance (KnownNat (GFieldSize g), KnownNat (GFieldSize f), GBitPack f, GBitPack g) => GBitPack (f :*: g) where
  type GFieldSize (f :*: g) = GFieldSize f + GFieldSize g
  type GConstructorCount (f :*: g) = 1

  gPackFields cc fg =
    (cc, packXWith go fg)
   where
    go (l0 :*: r0) =
      let (_, l1) = gPackFields cc l0 in
      let (_, r1) = gPackFields cc r0 in
      l1 ++# r1

  gUnpack c cc b =
    gUnpack c cc front :*: gUnpack c cc back
   where
    (front, back) = split# b

instance BitPack c => GBitPack (K1 i c) where
  type GFieldSize (K1 i c) = BitSize c
  type GConstructorCount (K1 i c)  = 1

  gPackFields cc (K1 i) = (cc, pack i)
  gUnpack _c _cc b      = K1 (unpack b)

instance GBitPack U1 where
  type GFieldSize U1 = 0
  type GConstructorCount U1 = 1

  gPackFields cc U1 = (cc, 0)
  gUnpack _c _cc _b = U1

-- Instances derived using Generic
instance ( BitPack a
         , BitPack b
         ) => BitPack (Either a b)

instance BitPack a => BitPack (Maybe a)

#if MIN_VERSION_base(4,12,0)
instance BitPack a => BitPack (Complex a)
instance BitPack a => BitPack (Down a)
#endif

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
deriveBitPackTuples ''BitPack ''BitSize 'pack 'unpack
