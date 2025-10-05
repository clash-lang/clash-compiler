{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2016-2017, Myrtle Software Ltd,
                  2021-2025, QBayLogic B.V.,
                  2022,      Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
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

module Clash.Class.BitPack.Internal where

import Prelude                        hiding (map)

import Data.Binary.IEEE754            (doubleToWord, floatToWord, wordToDouble,
                                       wordToFloat)

import Data.Char                      (chr, ord)
import Data.Complex                   (Complex)
import Data.Functor.Compose           (Compose)
import Data.Functor.Const             (Const)
import Data.Functor.Identity          (Identity)
import Data.Functor.Product           (Product)
import Data.Functor.Sum               (Sum)
import Data.Int
import Data.Ord                       (Down)
import Data.Proxy                     (Proxy)
import Data.Type.Bool                 (type (||))
import Data.Word
import Foreign.C.Types                (CUShort)
import GHC.Generics
import GHC.TypeLits                   (KnownNat, Nat, type (+), type (-))
import GHC.TypeLits.Extra             (CLog, Max)
import Numeric.Half                   (Half (..))

import Clash.Annotations.Primitive    (hasBlackBox)
import Clash.Class.BitPack.Internal.TH (deriveBitPackTuples)
import Clash.Class.Resize             (zeroExtend, resize)
import Clash.Promoted.Nat             (SNat(..), snatToNum)
import Clash.Sized.Internal.BitVector
  (pack#, split#, checkUnpackUndef, undefined#, unpack#, unsafeToNatural, isLike#,
   BitVector, Bit, (++#), xToBV)

{- $setup
>>> :m -Prelude
>>> :set -XDataKinds
>>> import Clash.Prelude
>>> import Data.Proxy
-}

-- | Convert data to/from a 'BitVector'. This allows functions to be defined
-- on the underlying representation of data, while exposing a nicer API using
-- 'pack' / 'unpack' at the boundaries. For example:
--
-- @
--     f :: forall a b. (BitPack a, BitPack b) => a -> b
--     f = unpack . go . pack
--      where
--       go :: BitVector (BitSize a) -> BitVector (BitSize b)
--       go = _ -- A function on the underlying bit vector
-- @
--
-- A type should only implement this class if it has a statically known size,
-- as otherwise it is not possible to determine how many bits are needed to
-- represent values. This means that types such as @[a]@ cannot have @BitPack@
-- instances, as even if @a@ has a statically known size, the length of the
-- list cannot be known in advance.
--
-- It is not possible to give data a custom bit representation by providing a
-- @BitPack@ instance. A @BitPack@ instance allows no creativity and should
-- always accurately reflect the bit representation of the data in HDL. You
-- should always @derive ('Generic', BitPack)@ unless you use a custom data
-- representation, in which case you should use
-- 'Clash.Annotations.BitRepresentation.Deriving.deriveBitPack'. Custom
-- encodings can be created with "Clash.Annotations.BitRepresentation" and
-- "Clash.Annotations.BitRepresentation.Deriving".
--
-- If the @BitPack@ instance does not accurately match the bit representation of
-- the data in HDL, Clash designs will exhibit incorrect behavior in various
-- places.
--
-- Clash provides some generic functions on packable types in the prelude, such
-- as indexing into packable structures (see "Clash.Class.BitPack.BitIndex") and
-- bitwise reduction of packable data (see "Clash.Class.BitPack.BitReduction").
--
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

  -- | Is 'True' for product types.
  type IsProductType a :: Bool
  type IsProductType a = GIsProductType (Rep a)

  -- | Is 'True' for sum types.
  type IsSumType a :: Bool
  type IsSumType a = GIsSumType (Rep a)

  -- | Convert element of type @a@ to a 'BitVector'
  --
  -- >>> pack (-5 :: Signed 6)
  -- 0b11_1011
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
  -- 0b11_1011
  -- >>> let x = pack (-5 :: Signed 6)
  -- >>> unpack x :: Unsigned 6
  -- 59
  -- >>> pack (59 :: Unsigned 6)
  -- 0b11_1011
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
packXWith f = xToBV . f
{-# INLINE packXWith #-}

-- | Pack both arguments to a 'BitVector' and use
-- 'Clash.Sized.Internal.BitVector.isLike#' to compare them. This is a more
-- lentiant comparison than '(==)', behaving more like (but not necessarily
-- exactly the same as) @std_match@ in VHDL or @casez@ in Verilog.
--
-- Unlike '(==)', isLike is not symmetric. The reason for this is that a
-- defined bit is said to be like an undefined bit, but not vice-versa:
--
-- >>> isLike (12 :: Signed 8) undefined
-- True
-- >>> isLike undefined (12 :: Signed 8)
-- False
--
-- However, it is still trivially reflexive and transitive:
--
-- >>> :set -XTemplateHaskell
-- >>> let x1 = $(bLit "0010")
-- >>> let x2 = $(bLit "0.10")
-- >>> let x3 = $(bLit "0.1.")
-- >>> isLike x1 x1
-- True
-- >>> isLike x1 x2
-- True
-- >>> isLike x2 x3
-- True
-- >>> isLike x1 x3
-- True
--
-- __NB__: Not synthesizable
--
isLike
  :: (BitPack a)
  => a
  -> a
  -> Bool
isLike x y =
  isLike# (pack x) (pack y)

{-# INLINE[1] bitCoerce #-}
-- | Coerce a value from one type to another through its bit representation.
--
-- >>> pack (-5 :: Signed 6)
-- 0b11_1011
-- >>> bitCoerce (-5 :: Signed 6) :: Unsigned 6
-- 59
-- >>> pack (59 :: Unsigned 6)
-- 0b11_1011
bitCoerce
  :: (BitPack a, BitPack b, BitSize a ~ BitSize b)
  => a
  -> b
bitCoerce = unpack . pack

-- | Map a value by first coercing to another type through its bit representation.
--
-- >>> pack (-5 :: Signed 32)
-- 0b1111_1111_1111_1111_1111_1111_1111_1011
-- >>> bitCoerceMap @(Vec 4 (BitVector 8)) (replace 1 0) (-5 :: Signed 32)
-- -16711685
-- >>> pack (-16711685 :: Signed 32)
-- 0b1111_1111_0000_0000_1111_1111_1111_1011
bitCoerceMap
  :: forall a b . (BitPack a, BitPack b, BitSize a ~ BitSize b)
  => (a -> a)
  -> b
  -> b
bitCoerceMap f = bitCoerce . f . bitCoerce

instance BitPack Bool where
  type BitSize Bool = 1
  type IsProductType Bool = False
  type IsSumType Bool = False
  pack   = let go b = if b then 1 else 0 in packXWith go
  unpack = checkUnpackUndef $ \bv -> if bv == 1 then True else False

instance KnownNat n => BitPack (BitVector n) where
  type BitSize (BitVector n) = n
  type IsProductType (BitVector n) = False
  type IsSumType (BitVector n) = False
  pack     = packXWith id
  unpack v = v

instance BitPack Bit where
  type BitSize Bit = 1
  type IsProductType Bit = False
  type IsSumType Bit = False
  pack   = packXWith pack#
  unpack = unpack#

instance BitPack Int where
  type BitSize Int = WORD_SIZE_IN_BITS
  type IsProductType Int = False
  type IsSumType Int = False
  pack   = packXWith fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Int8 where
  type BitSize Int8 = 8
  type IsProductType Int8 = False
  type IsSumType Int8 = False
  pack   = packXWith fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Int16 where
  type BitSize Int16 = 16
  type IsProductType Int16 = False
  type IsSumType Int16 = False
  pack   = packXWith fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Int32 where
  type BitSize Int32 = 32
  type IsProductType Int32 = False
  type IsSumType Int32 = False
  pack   = packXWith fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Int64 where
  type BitSize Int64 = 64
  type IsProductType Int64 = False
  type IsSumType Int64 = False
  pack   = packXWith fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Word where
  type BitSize Word = WORD_SIZE_IN_BITS
  type IsProductType Word = False
  type IsSumType Word = False
  pack   = packXWith fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Word8 where
  type BitSize Word8 = 8
  type IsProductType Word8 = False
  type IsSumType Word8 = False
  pack   = packXWith fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Word16 where
  type BitSize Word16 = 16
  type IsProductType Word16 = False
  type IsSumType Word16 = False
  pack   = packXWith fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Word32 where
  type BitSize Word32 = 32
  type IsProductType Word32 = False
  type IsSumType Word32 = False
  pack   = packXWith fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Word64 where
  type BitSize Word64 = 64
  type IsProductType Word64 = False
  type IsSumType Word64 = False
  pack   = packXWith fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Float where
  type BitSize Float = 32
  type IsProductType Float = False
  type IsSumType Float = False
  pack   = packXWith packFloat#
  unpack = checkUnpackUndef unpackFloat#

packFloat# :: Float -> BitVector 32
packFloat# = fromIntegral . floatToWord
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE packFloat# #-}
{-# ANN packFloat# hasBlackBox #-}

unpackFloat# :: BitVector 32 -> Float
unpackFloat# (unsafeToNatural -> w) = wordToFloat (fromIntegral w)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unpackFloat# #-}
{-# ANN unpackFloat# hasBlackBox #-}

instance BitPack Double where
  type BitSize Double = 64
  type IsProductType Double = False
  type IsSumType Double = False
  pack   = packXWith packDouble#
  unpack = checkUnpackUndef unpackDouble#

packDouble# :: Double -> BitVector 64
packDouble# = fromIntegral . doubleToWord
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE packDouble# #-}
{-# ANN packDouble# hasBlackBox #-}

unpackDouble# :: BitVector 64 -> Double
unpackDouble# (unsafeToNatural -> w) = wordToDouble (fromIntegral w)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unpackDouble# #-}
{-# ANN unpackDouble# hasBlackBox #-}

instance BitPack CUShort where
  type BitSize CUShort = 16
  type IsProductType CUShort = False
  type IsSumType CUShort = False
  pack   = packXWith fromIntegral
  unpack = checkUnpackUndef fromIntegral

instance BitPack Half where
  type BitSize Half = 16
  type IsProductType Half = False
  type IsSumType Half = False
  pack (Half x) = pack x
  unpack        = checkUnpackUndef $ \x -> Half (unpack x)

instance BitPack () where
  type BitSize () = 0
  type IsProductType () = False
  type IsSumType () = False
  pack   _ = minBound
  unpack _ = ()

instance BitPack Char where
  type BitSize Char = 21
  type IsProductType Char = False
  type IsSumType Char = False
  pack   = packXWith packChar#
  unpack = checkUnpackUndef unpackChar#

packChar# :: Char -> BitVector 21
packChar# = fromIntegral . ord
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE packChar# #-}
{-# ANN packChar# hasBlackBox #-}

unpackChar# :: BitVector 21 -> Char
unpackChar# = chr . fromIntegral
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unpackChar# #-}
{-# ANN unpackChar# hasBlackBox #-}

-- | __NB__: The documentation only shows instances up to /3/-tuples. By
-- default, instances up to and including /12/-tuples will exist. If the flag
-- @large-tuples@ is set instances up to the GHC imposed limit will exist. The
-- GHC imposed limit is either 62 or 64 depending on the GHC version.
instance (BitPack a, BitPack b) => BitPack (a,b) where
  type BitSize (a,b) = BitSize a + BitSize b
  type IsProductType (a,b) = True
  type IsSumType (a,b) = IsSumType a || IsSumType b
  pack = let go (a,b) = pack a ++# pack b in packXWith go
  unpack ab  = let (a,b) = split# ab in (unpack a, unpack b)

class GBitPack f where
  -- | Size of fields. If multiple constructors exist, this is the maximum of
  -- the sum of each of the constructors fields.
  type GFieldSize f :: Nat

  -- | Number of constructors this type has. Indirectly indicates how many bits
  -- are needed to represent the constructor.
  type GConstructorCount f :: Nat

  -- | Is 'True' for product types.
  type GIsProductType f :: Bool

  -- | Is 'True' for sum types.
  type GIsSumType f :: Bool

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
  type GIsProductType (M1 m d a) = GIsProductType a
  type GIsSumType (M1 m d a) = GIsSumType a

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
  type GIsProductType (f :+: g) = GIsProductType f || GIsProductType g
  type GIsSumType (f :+: g) = True

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
  type GIsProductType (f :*: g) = True
  type GIsSumType (f :*: g) = GIsSumType f || GIsSumType g

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
  type GIsProductType (K1 i c) = IsProductType c
  type GIsSumType (K1 i c) = IsSumType c

  gPackFields cc (K1 i) = (cc, pack i)
  gUnpack _c _cc b      = K1 (unpack b)

instance GBitPack U1 where
  type GFieldSize U1 = 0
  type GConstructorCount U1 = 1
  type GIsProductType U1 = False
  type GIsSumType U1 = False

  gPackFields cc U1 = (cc, 0)
  gUnpack _c _cc _b = U1

-- Instances derived using Generic
instance BitPack Ordering

instance ( BitPack a
         , BitPack b
         ) => BitPack (Either a b)

instance BitPack a => BitPack (Maybe a)

-- | Any 'Proxy' has a bit size of 0 and hence packs to @0 :: BitVector 0@.
--
-- >>> pack (Proxy @())
-- 0
-- >>> pack (Proxy @5)
-- 0
-- >>> pack (Proxy @Bool)
-- 0
instance BitPack (Proxy a)

instance BitPack a => BitPack (Complex a)
instance BitPack a => BitPack (Down a)

instance BitPack a => BitPack (Identity a)
instance BitPack a => BitPack (Const a b)
instance (BitPack (f a), BitPack (g a)) => BitPack (Product f g a)
instance (BitPack (f a), BitPack (g a)) => BitPack (Sum f g a)
instance BitPack (f (g a)) => BitPack (Compose f g a)

-- | Zero-extend a 'Bool'ean value to a 'BitVector' of the appropriate size.
--
-- >>> boolToBV True :: BitVector 6
-- 0b00_0001
-- >>> boolToBV False :: BitVector 6
-- 0b00_0000
boolToBV :: KnownNat n => Bool -> BitVector (n + 1)
boolToBV = zeroExtend . pack

-- | Convert a Bool to a Bit
boolToBit :: Bool -> Bit
boolToBit = bitCoerce

-- | Convert a Bit to a Bool
bitToBool :: Bit -> Bool
bitToBool = bitCoerce

-- Derive the BitPack instance for tuples of size 3 to maxTupleSize
deriveBitPackTuples ''BitPack ''BitSize ''IsProductType ''IsSumType
                    'pack 'unpack
