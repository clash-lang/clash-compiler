{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2016     , Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

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
import GHC.Generics                   (Generic)
import GHC.TypeLits                   (KnownNat, Nat, type (+), natVal)
import GHC.TypeLits.Extra             (Max)
import Language.Haskell.TH            (TypeQ, appT, conT, litT, numTyLit, sigE)
import Language.Haskell.TH.Syntax     (Lift(..))
import Numeric.Natural                (Natural)
import Test.QuickCheck.Arbitrary      (Arbitrary (..), CoArbitrary (..),
                                       arbitraryBoundedIntegral,
                                       coarbitraryIntegral)

import Clash.Class.BitPack            (BitPack (..), packXWith)
import Clash.Class.Num                (ExtendingNum (..), SaturatingNum (..),
                                       SaturationMode (..))
import Clash.Class.Parity             (Parity (..))
import Clash.Class.Resize             (Resize (..))
import Clash.Prelude.BitIndex         ((!), msb, replaceBit, split)
import Clash.Prelude.BitReduction     (reduceOr)
import Clash.Sized.Internal.BitVector (BitVector (BV), Bit, high, low, undefError)
import qualified Clash.Sized.Internal.BitVector as BV
import Clash.XException
  (ShowX (..), NFDataX (..), errorX, showsPrecXWith, rwhnfX)

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
newtype Unsigned (n :: Nat) =
    -- | The constructor, 'U', and the field, 'unsafeToInteger', are not
    -- synthesizable.
    U { unsafeToInteger :: Integer }
  deriving (Data, Generic)

{-# NOINLINE size# #-}
size# :: KnownNat n => Unsigned n -> Int
size# u = fromInteger (natVal u)

instance NFData (Unsigned n) where
  rnf (U i) = rnf i `seq` ()
  {-# NOINLINE rnf #-}
  -- NOINLINE is needed so that Clash doesn't trip on the "Unsigned ~# Integer"
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
  succ           = (+# fromInteger# 1)
  pred           = (-# fromInteger# 1)
  toEnum         = fromInteger# . toInteger
  fromEnum       = fromEnum . toInteger#
  enumFrom       = enumFrom#
  enumFromThen   = enumFromThen#
  enumFromTo     = enumFromTo#
  enumFromThenTo = enumFromThenTo#

{-# NOINLINE enumFrom# #-}
{-# NOINLINE enumFromThen# #-}
{-# NOINLINE enumFromTo# #-}
{-# NOINLINE enumFromThenTo# #-}
enumFrom#       :: KnownNat n => Unsigned n -> [Unsigned n]
enumFromThen#   :: KnownNat n => Unsigned n -> Unsigned n -> [Unsigned n]
enumFromTo#     :: Unsigned n -> Unsigned n -> [Unsigned n]
enumFromThenTo# :: Unsigned n -> Unsigned n -> Unsigned n -> [Unsigned n]
enumFrom# x             = map fromInteger_INLINE [unsafeToInteger x ..]
enumFromThen# x y       = map fromInteger_INLINE [unsafeToInteger x, unsafeToInteger y ..]
enumFromTo# x y         = map U [unsafeToInteger x .. unsafeToInteger y]
enumFromThenTo# x1 x2 y = map U [unsafeToInteger x1, unsafeToInteger x2 .. unsafeToInteger y]

instance KnownNat n => Bounded (Unsigned n) where
  minBound = minBound#
  maxBound = maxBound#

{-# NOINLINE minBound# #-}
minBound# :: Unsigned n
minBound# = U 0

{-# NOINLINE maxBound# #-}
maxBound# :: forall n .KnownNat n => Unsigned n
maxBound# = let m = 1 `shiftL` fromInteger (natVal (Proxy @n))
            in  U (m - 1)

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
(+#) (U i) (U j) = let m = 1 `shiftL` fromInteger (natVal (Proxy @n))
                       z = i + j
                   in  if z >= m then U (z - m) else U z

{-# NOINLINE (-#) #-}
(-#) (U i) (U j) = let m = 1 `shiftL` fromInteger (natVal (Proxy @n))
                       z = i - j
                   in  if z < 0 then U (m + z) else U z

{-# NOINLINE (*#) #-}
(*#) (U i) (U j) = fromInteger_INLINE (i * j)

{-# NOINLINE negate# #-}
negate# :: forall n . KnownNat n => Unsigned n -> Unsigned n
negate# (U 0) = U 0
negate# (U i) = sz `seq` U (sz - i)
  where
    sz = 1 `shiftL` fromInteger (natVal (Proxy @n))

{-# NOINLINE fromInteger# #-}
fromInteger# :: KnownNat n => Integer -> Unsigned n
fromInteger# = fromInteger_INLINE

{-# INLINE fromInteger_INLINE #-}
fromInteger_INLINE :: forall n . KnownNat n => Integer -> Unsigned n
fromInteger_INLINE i = U (i `mod` sz)
  where
    sz = 1 `shiftL` fromInteger (natVal (Proxy @n))

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
minus# (U a) (U b) =
  let sz   = fromInteger (natVal (Proxy @(Max m n + 1)))
      mask = 1 `shiftL` sz
      z    = a - b
  in  if z < 0 then U (mask + z) else U z

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
toInteger# (U i) = i

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
complement# :: KnownNat n => Unsigned n -> Unsigned n
complement# (U i) = fromInteger_INLINE (complement i)

shiftL#, shiftR#, rotateL#, rotateR# :: KnownNat n => Unsigned n -> Int -> Unsigned n
{-# NOINLINE shiftL# #-}
shiftL# (U v) i
  | i < 0     = error
              $ "'shiftL undefined for negative number: " ++ show i
  | otherwise = fromInteger_INLINE (shiftL v i)

{-# NOINLINE shiftR# #-}
-- shiftR# doesn't need the KnownNat constraint
-- But having the same type signature for all shift and rotate functions
-- makes implementing the Evaluator easier.
shiftR# (U v) i
  | i < 0     = error
              $ "'shiftR undefined for negative number: " ++ show i
  | otherwise = U (shiftR v i)

{-# NOINLINE rotateL# #-}
rotateL# _ b | b < 0 = error "'shiftL undefined for negative numbers"
rotateL# bv@(U n) b   = fromInteger_INLINE (l .|. r)
  where
    l    = shiftL n b'
    r    = shiftR n b''

    b'   = b `mod` sz
    b''  = sz - b'
    sz   = fromInteger (natVal bv)

{-# NOINLINE rotateR# #-}
rotateR# _ b | b < 0 = error "'shiftR undefined for negative numbers"
rotateR# bv@(U n) b   = fromInteger_INLINE (l .|. r)
  where
    l   = shiftR n b'
    r   = shiftL n b''

    b'  = b `mod` sz
    b'' = sz - b'
    sz  = fromInteger (natVal bv)

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
resize# (U i) = let m = 1 `shiftL` fromInteger (natVal (Proxy @m))
                in  if i >= m then fromInteger_INLINE i else U i

instance Default (Unsigned n) where
  def = minBound#

instance KnownNat n => Lift (Unsigned n) where
  lift u@(U i) = sigE [| fromInteger# i |] (decUnsigned (natVal u))
  {-# NOINLINE lift #-}

decUnsigned :: Integer -> TypeQ
decUnsigned n = appT (conT ''Unsigned) (litT $ numTyLit n)

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
