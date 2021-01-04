{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2016     , Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE Unsafe #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK show-extensions not-home #-}

module Clash.Sized.Internal.Unsigned
  ( -- * Datatypes
    Unsigned (..)
    -- * Accessors
    -- ** Length information
  , size#
    -- * Type classes
    -- ** BitPack
  , pack#
  , unpack#
    -- ** Eq
  , eq#
  , neq#
    -- ** Ord
  , lt#
  , ge#
  , gt#
  , le#
    -- ** Enum (not synthesizable)
  , enumFrom#
  , enumFromThen#
  , enumFromTo#
  , enumFromThenTo#
    -- ** Bounded
  , minBound#
  , maxBound#
    -- ** Num
  , (+#)
  , (-#)
  , (*#)
  , negate#
  , fromInteger#
    -- ** ExtendingNum
  , plus#
  , minus#
  , times#
    -- ** Integral
  , quot#
  , rem#
  , toInteger#
    -- ** Bits
  , and#
  , or#
  , xor#
  , complement#
  , shiftL#
  , shiftR#
  , rotateL#
  , rotateR#
    -- ** Resize
  , resize#
    -- ** Conversions
  , unsignedToWord
  , unsigned8toWord8
  , unsigned16toWord16
  , unsigned32toWord32
  )
where

import Prelude hiding                 (even, odd)

import Control.DeepSeq                (NFData (..))
import Control.Lens                   (Index, Ixed (..), IxValue)
import Data.Bits                      (Bits (..), FiniteBits (..))
import Data.Data                      (Data)
import Data.Default.Class             (Default (..))
import Data.Proxy                     (Proxy (..))
import Text.Read                      (Read (..), ReadPrec)
import Text.Printf                    (PrintfArg (..), printf)
import GHC.Exts                       (narrow8Word#, narrow16Word#, narrow32Word#)
import GHC.Generics                   (Generic)
#if MIN_VERSION_base(4,15,0)
import GHC.Num.BigNat                 (bigNatToWord, bigNatToWord#)
import GHC.Num.Integer
  (integerFromNatural, integerShiftL, integerToNatural)
import GHC.Num.Natural
  (Natural (..), naturalShiftL, naturalShiftR, naturalToWord)
#else
import GHC.Integer.GMP.Internals      (bigNatToWord)
import GHC.Natural                    (Natural (..), naturalFromInteger)
#endif
#if MIN_VERSION_base(4,12,0)
import GHC.Natural                    (naturalToInteger)
#endif
import GHC.TypeLits                   (KnownNat, Nat, type (+))
#if MIN_VERSION_base(4,15,0)
import GHC.TypeNats                   (natVal)
#else
import GHC.TypeLits                   (natVal)
#endif
import GHC.TypeLits.Extra             (Max)
import GHC.Word                       (Word (..), Word8 (..), Word16 (..), Word32 (..))
import Data.Ix                        (Ix(..))
import Language.Haskell.TH            (appT, conT, litT, numTyLit, sigE)
import Language.Haskell.TH.Syntax     (Lift(..))
#if MIN_VERSION_template_haskell(2,16,0)
import Language.Haskell.TH.Compat
#endif
#if MIN_VERSION_template_haskell(2,17,0)
import Language.Haskell.TH            (Quote, Type)
#else
import Language.Haskell.TH            (TypeQ)
#endif
import Test.QuickCheck.Arbitrary      (Arbitrary (..), CoArbitrary (..),
                                       arbitraryBoundedIntegral,
                                       coarbitraryIntegral)

import Clash.Class.BitPack            (BitPack (..), packXWith, bitCoerce)
import Clash.Class.Num                (ExtendingNum (..), SaturatingNum (..),
                                       SaturationMode (..))
import Clash.Class.Parity             (Parity (..))
import Clash.Class.Resize             (Resize (..))
import Clash.Prelude.BitIndex         ((!), msb, replaceBit, split)
import Clash.Prelude.BitReduction     (reduceOr)
import Clash.Promoted.Nat             (natToNum, natToNatural)
import Clash.Sized.Internal.BitVector (BitVector (BV), Bit, high, low, undefError)
import qualified Clash.Sized.Internal.BitVector as BV
import Clash.Sized.Internal.Mod
import Clash.XException
  (ShowX (..), NFDataX (..), errorX, showsPrecXWith, rwhnfX)

#include "MachDeps.h"

-- | Arbitrary-width unsigned integer represented by @n@ bits
--
-- Given @n@ bits, an 'Unsigned' @n@ number has a range of: [0 .. 2^@n@-1]
--
-- __NB__: The 'Num' operators perform @wrap-around@ on overflow. If you want
-- saturation on overflow, check out the 'SaturatingNum' class.
--
-- >>> maxBound :: Unsigned 3
-- 7
-- >>> minBound :: Unsigned 3
-- 0
-- >>> read (show (maxBound :: Unsigned 3)) :: Unsigned 3
-- 7
-- >>> 1 + 2 :: Unsigned 3
-- 3
-- >>> 2 + 6 :: Unsigned 3
-- 0
-- >>> 1 - 3 :: Unsigned 3
-- 6
-- >>> 2 * 3 :: Unsigned 3
-- 6
-- >>> 2 * 4 :: Unsigned 3
-- 0
-- >>> (2 :: Unsigned 3) `mul` (4 :: Unsigned 3) :: Unsigned 6
-- 8
-- >>> (2 :: Unsigned 3) `add` (6 :: Unsigned 3) :: Unsigned 4
-- 8
-- >>> satAdd SatSymmetric 2 6 :: Unsigned 3
-- 7
-- >>> satSub SatSymmetric 2 3 :: Unsigned 3
-- 0
#if MIN_VERSION_base(4,15,0)
data Unsigned (n :: Nat) =
    -- | The constructor, 'U', and the field, 'unsafeToInteger', are not
    -- synthesizable.
    U { unsafeToNatural :: !Natural }
#else
newtype Unsigned (n :: Nat) =
    -- | The constructor, 'U', and the field, 'unsafeToInteger', are not
    -- synthesizable.
    U { unsafeToNatural :: Natural }
#endif
  deriving (Data, Generic)

{-# NOINLINE size# #-}
size# :: KnownNat n => Unsigned n -> Int
#if MIN_VERSION_base(4,15,0)
size# u = fromIntegral (natVal u)
#else
size# u = fromInteger (natVal u)
#endif

instance NFData (Unsigned n) where
  rnf (U i) = rnf i `seq` ()
  {-# NOINLINE rnf #-}
  -- NOINLINE is needed so that Clash doesn't trip on the "Unsigned ~# Natural"
  -- coercion

instance Show (Unsigned n) where
  show (U i) = show i
  {-# NOINLINE show #-}

instance ShowX (Unsigned n) where
  showsPrecX = showsPrecXWith showsPrec

instance NFDataX (Unsigned n) where
  deepErrorX = errorX
  rnfX = rwhnfX

-- | None of the 'Read' class' methods are synthesizable.
instance KnownNat n => Read (Unsigned n) where
  readPrec = fromIntegral <$> (readPrec :: ReadPrec Natural)

instance KnownNat n => BitPack (Unsigned n) where
  type BitSize (Unsigned n) = n
  pack   = packXWith pack#
  unpack = unpack#

{-# NOINLINE pack# #-}
pack# :: Unsigned n -> BitVector n
pack# (U i) = BV 0 i

{-# NOINLINE unpack# #-}
unpack# :: KnownNat n => BitVector n -> Unsigned n
unpack# (BV 0 i) = U i
unpack# bv = undefError "Unsigned.unpack" [bv]

instance Eq (Unsigned n) where
  (==) = eq#
  (/=) = neq#

{-# NOINLINE eq# #-}
eq# :: Unsigned n -> Unsigned n -> Bool
eq# (U v1) (U v2) = v1 == v2

{-# NOINLINE neq# #-}
neq# :: Unsigned n -> Unsigned n -> Bool
neq# (U v1) (U v2) = v1 /= v2

instance Ord (Unsigned n) where
  (<)  = lt#
  (>=) = ge#
  (>)  = gt#
  (<=) = le#

lt#,ge#,gt#,le# :: Unsigned n -> Unsigned n -> Bool
{-# NOINLINE lt# #-}
lt# (U n) (U m) = n < m
{-# NOINLINE ge# #-}
ge# (U n) (U m) = n >= m
{-# NOINLINE gt# #-}
gt# (U n) (U m) = n > m
{-# NOINLINE le# #-}
le# (U n) (U m) = n <= m

-- | The functions: 'enumFrom', 'enumFromThen', 'enumFromTo', and
-- 'enumFromThenTo', are not synthesizable.
instance KnownNat n => Enum (Unsigned n) where
  succ n
    | n == maxBound =
        error $ "'succ' was called on (" <> show @(Unsigned n) maxBound <> " :: "
             <> "Unsigned " <> show (natToNatural @n) <> ") and caused an "
             <> "overflow. Use 'satSucc' and specify a SaturationMode if you "
             <> "need other behavior."
    | otherwise = n +# fromInteger# 1

  pred n
    | n == minBound =
        error $ "'pred' was called on (" <> show @(Unsigned n) maxBound <> " :: "
             <> "Unsigned " <> show (natToNatural @n) <> ") and caused an "
             <> "underflow. Use 'satPred' and specify a SaturationMode if you "
             <> "need other behavior."
    | otherwise = n -# fromInteger# 1

  toEnum         = fromInteger# . toInteger
  fromEnum       = fromEnum . toInteger#
  enumFrom       = enumFrom#
  enumFromThen   = enumFromThen#
  enumFromTo     = enumFromTo#
  enumFromThenTo = enumFromThenTo#

enumFrom# :: forall n. KnownNat n => Unsigned n -> [Unsigned n]
enumFrom# = \x -> map (U . (`mod` m)) [unsafeToNatural x .. unsafeToNatural (maxBound :: Unsigned n)]
#if MIN_VERSION_base(4,15,0)
  where m = 1 `naturalShiftL` naturalToWord (natVal (Proxy @n))
#else
  where m = 1 `shiftL` fromInteger (natVal (Proxy @n))
#endif
{-# NOINLINE enumFrom# #-}

enumFromThen# :: forall n. KnownNat n => Unsigned n -> Unsigned n -> [Unsigned n]
enumFromThen# = \x y -> toUnsigneds [unsafeToNatural x, unsafeToNatural y .. bound x y]
 where
  toUnsigneds = map (U . (`mod` m))
  bound x y = unsafeToNatural (if x <= y then maxBound else minBound :: Unsigned n)
#if MIN_VERSION_base(4,15,0)
  m = 1 `naturalShiftL` naturalToWord (natVal (Proxy @n))
#else
  m = 1 `shiftL` fromInteger (natVal (Proxy @n))
#endif
{-# NOINLINE enumFromThen# #-}

enumFromTo# :: forall n. KnownNat n => Unsigned n -> Unsigned n -> [Unsigned n]
enumFromTo# = \x y -> map (U . (`mod` m)) [unsafeToNatural x .. unsafeToNatural y]
#if MIN_VERSION_base(4,15,0)
  where m = 1 `naturalShiftL` naturalToWord (natVal (Proxy @n))
#else
  where m = 1 `shiftL` fromInteger (natVal (Proxy @n))
#endif
{-# NOINLINE enumFromTo# #-}

enumFromThenTo# :: forall n. KnownNat n => Unsigned n -> Unsigned n -> Unsigned n -> [Unsigned n]
enumFromThenTo# = \x1 x2 y -> map (U . (`mod` m)) [unsafeToNatural x1, unsafeToNatural x2 .. unsafeToNatural y]
#if MIN_VERSION_base(4,15,0)
  where m = 1 `naturalShiftL` naturalToWord (natVal (Proxy @n))
#else
  where m = 1 `shiftL` fromInteger (natVal (Proxy @n))
#endif
{-# NOINLINE enumFromThenTo# #-}

instance KnownNat n => Bounded (Unsigned n) where
  minBound = minBound#
  maxBound = maxBound#

minBound# :: Unsigned n
minBound# = U 0
{-# NOINLINE minBound# #-}

maxBound# :: forall n. KnownNat n => Unsigned n
maxBound# = let m = 1 `shiftL` (natToNum @n) in  U (m - 1)
{-# NOINLINE maxBound# #-}

instance KnownNat n => Num (Unsigned n) where
  (+)         = (+#)
  (-)         = (-#)
  (*)         = (*#)
  negate      = negate#
  abs         = id
  signum bv   = resize# (unpack# (BV.pack# (reduceOr bv)))
  fromInteger = fromInteger#

(+#),(-#),(*#) :: forall n . KnownNat n => Unsigned n -> Unsigned n -> Unsigned n
{-# NOINLINE (+#) #-}
(+#) = \(U i) (U j) -> U (addMod m i j)
#if MIN_VERSION_base(4,15,0)
  where m = 1 `naturalShiftL` naturalToWord (natVal (Proxy @n))
#else
  where m = 1 `shiftL` fromInteger (natVal (Proxy @n))
#endif

{-# NOINLINE (-#) #-}
(-#) = \(U i) (U j) -> U (subMod m i j)
#if MIN_VERSION_base(4,15,0)
  where m = 1 `naturalShiftL` naturalToWord (natVal (Proxy @n))
#else
  where m = 1 `shiftL` fromInteger (natVal (Proxy @n))
#endif

{-# NOINLINE (*#) #-}
(*#) = \(U i) (U j) -> U (mulMod2 m i j)
#if MIN_VERSION_base(4,15,0)
  where m = (1 `naturalShiftL` naturalToWord (natVal (Proxy @n))) - 1
#else
  where m = (1 `shiftL` fromInteger (natVal (Proxy @n))) - 1
#endif

{-# NOINLINE negate# #-}
negate# :: forall n . KnownNat n => Unsigned n -> Unsigned n
negate# = \(U i) -> U (negateMod m i)
#if MIN_VERSION_base(4,15,0)
  where m = 1 `naturalShiftL` naturalToWord (natVal (Proxy @n))
#else
  where m = 1 `shiftL` fromInteger (natVal (Proxy @n))
#endif

{-# NOINLINE fromInteger# #-}
fromInteger# :: forall n . KnownNat n => Integer -> Unsigned n
#if MIN_VERSION_base(4,15,0)
fromInteger# = \x -> U (integerToNatural (x `mod` m))
 where
  m = 1 `integerShiftL` naturalToWord (natVal (Proxy @n))
#else
fromInteger# = \x -> U (naturalFromInteger (x `mod` m))
 where
  m = 1 `shiftL` fromInteger (natVal (Proxy @n))
#endif

instance (KnownNat m, KnownNat n) => ExtendingNum (Unsigned m) (Unsigned n) where
  type AResult (Unsigned m) (Unsigned n) = Unsigned (Max m n + 1)
  add  = plus#
  sub = minus#
  type MResult (Unsigned m) (Unsigned n) = Unsigned (m + n)
  mul = times#

{-# NOINLINE plus# #-}
plus# :: Unsigned m -> Unsigned n -> Unsigned (Max m n + 1)
plus# (U a) (U b) = U (a + b)

{-# NOINLINE minus# #-}
minus# :: forall m n . (KnownNat m, KnownNat n) => Unsigned m -> Unsigned n
                                                -> Unsigned (Max m n + 1)
minus# = \(U a) (U b) -> U (subMod mask a b)
 where
#if MIN_VERSION_base(4,15,0)
  sz   = naturalToWord (natVal (Proxy @(Max m n + 1)))
  mask = 1 `naturalShiftL` sz
#else
  sz   = fromInteger (natVal (Proxy @(Max m n + 1)))
  mask = 1 `shiftL` sz
#endif

{-# NOINLINE times# #-}
times# :: Unsigned m -> Unsigned n -> Unsigned (m + n)
times# (U a) (U b) = U (a * b)

instance KnownNat n => Real (Unsigned n) where
  toRational = toRational . toInteger#

instance KnownNat n => Integral (Unsigned n) where
  quot        = quot#
  rem         = rem#
  div         = quot#
  mod         = rem#
  quotRem n d = (n `quot#` d,n `rem#` d)
  divMod  n d = (n `quot#` d,n `rem#` d)
  toInteger   = toInteger#

quot#,rem# :: Unsigned n -> Unsigned n -> Unsigned n
{-# NOINLINE quot# #-}
quot# (U i) (U j) = U (i `quot` j)
{-# NOINLINE rem# #-}
rem# (U i) (U j) = U (i `rem` j)

{-# NOINLINE toInteger# #-}
toInteger# :: Unsigned n -> Integer
toInteger# (U i) = naturalToInteger i

instance KnownNat n => PrintfArg (Unsigned n) where
  formatArg = formatArg . toInteger

instance KnownNat n => Parity (Unsigned n) where
  even = even . pack
  odd = odd . pack

instance KnownNat n => Bits (Unsigned n) where
  (.&.)             = and#
  (.|.)             = or#
  xor               = xor#
  complement        = complement#
  zeroBits          = 0
  bit i             = replaceBit i high 0
  setBit v i        = replaceBit i high v
  clearBit v i      = replaceBit i low  v
  complementBit v i = replaceBit i (BV.complement## (v ! i)) v
  testBit v i       = v ! i == high
  bitSizeMaybe v    = Just (size# v)
  bitSize           = size#
  isSigned _        = False
  shiftL v i        = shiftL# v i
  shiftR v i        = shiftR# v i
  rotateL v i       = rotateL# v i
  rotateR v i       = rotateR# v i
  popCount u        = popCount (pack# u)

{-# NOINLINE and# #-}
and# :: Unsigned n -> Unsigned n -> Unsigned n
and# (U v1) (U v2) = U (v1 .&. v2)

{-# NOINLINE or# #-}
or# :: Unsigned n -> Unsigned n -> Unsigned n
or# (U v1) (U v2) = U (v1 .|. v2)

{-# NOINLINE xor# #-}
xor# :: Unsigned n -> Unsigned n -> Unsigned n
xor# (U v1) (U v2) = U (v1 `xor` v2)

{-# NOINLINE complement# #-}
complement# :: forall n . KnownNat n => Unsigned n -> Unsigned n
complement# = \(U i) -> U (complementN i)
  where complementN = complementMod (natVal (Proxy @n))

shiftL#, shiftR#, rotateL#, rotateR# :: forall n .KnownNat n => Unsigned n -> Int -> Unsigned n
{-# NOINLINE shiftL# #-}
shiftL# =
  \(U v) i ->
    if i >= 0 then
#if MIN_VERSION_base
      U ((naturalShiftL v i) `mod` m)
#else
      U ((shiftL v i) `mod` m)
#endif
    else
      error ("'shiftL undefined for negative number: " ++ show i)
 where
#if MIN_VERSION_base(4,15,0)
  m = 1 `naturalShiftL` naturalToWord (natVal (Proxy @n))
#else
  m = 1 `shiftL` fromInteger (natVal (Proxy @n))
#endif

{-# NOINLINE shiftR# #-}
-- shiftR# doesn't need the KnownNat constraint
-- But having the same type signature for all shift and rotate functions
-- makes implementing the Evaluator easier.
shiftR# (U v) i
  | i < 0     = error
              $ "'shiftR undefined for negative number: " ++ show i
  | otherwise = U (shiftR v i)

{-# NOINLINE rotateL# #-}
rotateL# =
  \(U n) b ->
    if b >= 0 then
#if MIN_VERSION_base(4,15,0)
      let l   = naturalShiftL n b'
          r   = naturalShiftR n b''
          b'  = fromIntegral b `mod` sz
#else
      let l   = shiftL n b'
          r   = shiftR n b''
          b'  = b `mod` sz
#endif
          b'' = sz - b'
      in  U ((l .|. r) `mod` m)
    else
      error "'rotateL undefined for negative numbers"
  where
#if MIN_VERSION_base(4,15,0)
    sz = naturalToWord (natVal (Proxy @n))
    m  = 1 `naturalShiftL` sz
#else
    sz = fromInteger (natVal (Proxy @n)) :: Int
    m  = 1 `shiftL` sz
#endif

{-# NOINLINE rotateR# #-}
rotateR# =
  \(U n) b ->
    if b >= 0 then
#if MIN_VERSION_base(4,15,0)
      let l   = naturalShiftR n b'
          r   = naturalShiftL n b''
          b'  = fromIntegral b `mod` sz
#else
      let l   = shiftR n b'
          r   = shiftL n b''
          b'  = b `mod` sz
#endif
          b'' = sz - b'
      in  U ((l .|. r) `mod` m)
    else
      error "'rotateR undefined for negative numbers"
  where
#if MIN_VERSION_base(4,15,0)
    sz = naturalToWord (natVal (Proxy @n))
    m  = 1 `naturalShiftL` sz
#else
    sz = fromInteger (natVal (Proxy @n)) :: Int
    m  = 1 `shiftL` sz
#endif


instance KnownNat n => FiniteBits (Unsigned n) where
  finiteBitSize        = size#
  countLeadingZeros  u = countLeadingZeros  (pack# u)
  countTrailingZeros u = countTrailingZeros (pack# u)

instance Resize Unsigned where
  resize     = resize#
  zeroExtend = extend
  truncateB  = resize#

{-# NOINLINE resize# #-}
resize# :: forall n m . KnownNat m => Unsigned n -> Unsigned m
resize# = \(U i) -> if i >= m then U (i `mod` m) else U i
#if MIN_VERSION_base(4,15,0)
  where m = 1 `naturalShiftL` naturalToWord (natVal (Proxy @m))
#else
  where m = 1 `shiftL` fromInteger (natVal (Proxy @m))
#endif

instance Default (Unsigned n) where
  def = minBound#

instance KnownNat n => Lift (Unsigned n) where
  lift u@(U i) = sigE [| fromInteger# i |] (decUnsigned (natVal u))
  {-# NOINLINE lift #-}
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = liftTypedFromUntyped
#endif

#if MIN_VERSION_template_haskell(2,17,0)
decUnsigned :: Quote m => Natural -> m Type
decUnsigned n = appT (conT ''Unsigned) (litT $ numTyLit (integerFromNatural n))
#else
decUnsigned :: Integer -> TypeQ
decUnsigned n = appT (conT ''Unsigned) (litT $ numTyLit n)
#endif

instance KnownNat n => SaturatingNum (Unsigned n) where
  satAdd SatWrap a b = a +# b
  satAdd SatZero a b =
    let r = plus# a b
    in  case msb r of
          0 -> resize# r
          _ -> minBound#
  satAdd _ a b =
    let r  = plus# a b
    in  case msb r of
          0 -> resize# r
          _ -> maxBound#

  satSub SatWrap a b = a -# b
  satSub _ a b =
    let r = minus# a b
    in  case msb r of
          0 -> resize# r
          _ -> minBound#

  satMul SatWrap a b = a *# b
  satMul SatZero a b =
    let r       = times# a b
        (rL,rR) = split r
    in  case rL of
          0 -> unpack# rR
          _ -> minBound#
  satMul _ a b =
    let r       = times# a b
        (rL,rR) = split r
    in  case rL of
          0 -> unpack# rR
          _ -> maxBound#

instance KnownNat n => Arbitrary (Unsigned n) where
  arbitrary = arbitraryBoundedIntegral
  shrink    = BV.shrinkSizedUnsigned

instance KnownNat n => CoArbitrary (Unsigned n) where
  coarbitrary = coarbitraryIntegral

type instance Index   (Unsigned n) = Int
type instance IxValue (Unsigned n) = Bit
instance KnownNat n => Ixed (Unsigned n) where
  ix i f s = unpack# <$> BV.replaceBit# (pack# s) i
                     <$> f (BV.index# (pack# s) i)

instance (KnownNat n) => Ix (Unsigned n) where
  range (a, b) = [a..b]
  index ab@(a, b) x
    | inRange ab x = fromIntegral $ x - a
    | otherwise = error $ printf "Index %d out of bounds (%d, %d) ab" x a b
  inRange (a, b) x = a <= x && x <= b

unsignedToWord :: Unsigned WORD_SIZE_IN_BITS -> Word
#if MIN_VERSION_base(4,15,0)
unsignedToWord (U (NS u#)) = W# u#
unsignedToWord (U (NB u#)) = bigNatToWord u#
#else
unsignedToWord (U (NatS# u#)) = W# u#
unsignedToWord (U (NatJ# u#)) = W# (bigNatToWord u#)
#endif
{-# NOINLINE unsignedToWord #-}

unsigned8toWord8 :: Unsigned 8 -> Word8
#if MIN_VERSION_base(4,15,0)
unsigned8toWord8 (U (NS u#)) = W8# (narrow8Word# u#)
unsigned8toWord8 (U (NB u#)) = W8# (narrow8Word# (bigNatToWord# u#))
#else
unsigned8toWord8 (U (NatS# u#)) = W8# (narrow8Word# u#)
unsigned8toWord8 (U (NatJ# u#)) = W8# (narrow8Word# (bigNatToWord u#))
#endif
{-# NOINLINE unsigned8toWord8 #-}

unsigned16toWord16 :: Unsigned 16 -> Word16
#if MIN_VERSION_base(4,15,0)
unsigned16toWord16 (U (NS u#)) = W16# (narrow16Word# u#)
unsigned16toWord16 (U (NB u#)) = W16# (narrow16Word# (bigNatToWord# u#))
#else
unsigned16toWord16 (U (NatS# u#)) = W16# (narrow16Word# u#)
unsigned16toWord16 (U (NatJ# u#)) = W16# (narrow16Word# (bigNatToWord u#))
#endif
{-# NOINLINE unsigned16toWord16 #-}

unsigned32toWord32 :: Unsigned 32 -> Word32
#if MIN_VERSION_base(4,15,0)
unsigned32toWord32 (U (NS u#)) = W32# (narrow32Word# u#)
unsigned32toWord32 (U (NB u#)) = W32# (narrow32Word# (bigNatToWord# u#))
#else
unsigned32toWord32 (U (NatS# u#)) = W32# (narrow32Word# u#)
unsigned32toWord32 (U (NatJ# u#)) = W32# (narrow32Word# (bigNatToWord u#))
#endif
{-# NOINLINE unsigned32toWord32 #-}

{-# RULES
"bitCoerce/Unsigned WORD_SIZE_IN_BITS -> Word" bitCoerce = unsignedToWord
"bitCoerce/Unsigned 8 -> Word8" bitCoerce = unsigned8toWord8
"bitCoerce/Unsigned 16 -> Word16" bitCoerce = unsigned16toWord16
"bitCoerce/Unsigned 32 -> Word32" bitCoerce = unsigned32toWord32
 #-}
