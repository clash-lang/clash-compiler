{-|
Copyright  :  (C) 2013-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# LANGUAGE Unsafe #-}

{-# OPTIONS_HADDOCK show-extensions #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module CLaSH.Sized.Internal.Signed
  ( -- * Datatypes
    Signed (..)
    -- * Accessors
    -- ** Length information
  , size#
    -- * Type classes
    -- ** BitConvert
  , pack#
  , unpack#
    -- Eq
  , eq#
  , neq#
    -- ** Ord
  , lt#
  , ge#
  , gt#
  , le#
    -- ** Enum (not synthesisable)
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
  , abs#
  , fromInteger#
    -- ** ExtendingNum
  , plus#
  , minus#
  , times#
    -- ** Integral
  , quot#
  , rem#
  , div#
  , mod#
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
  , truncateB#
    -- ** SaturatingNum
  , minBoundSym#
  )
where

import Control.DeepSeq                (NFData (..))
import Control.Lens                   (Index, Ixed (..), IxValue)
import Data.Bits                      (Bits (..), FiniteBits (..))
import Data.Data                      (Data)
import Data.Default                   (Default (..))
import Data.Promotion.Prelude         (ConstSym1, FlipSym1)
import Data.Proxy                     (Proxy (..))
import Data.Singletons.Prelude        (Apply, TyFun)
import Text.Read                      (Read (..), ReadPrec)
import GHC.Exts                       (Constraint)
import GHC.TypeLits                   (KnownNat, Nat, type (+), type (-), type (^), natVal)
import Language.Haskell.TH            (TypeQ, appT, conT, litT, numTyLit, sigE)
import Language.Haskell.TH.Syntax     (Lift(..))
import Test.QuickCheck.Arbitrary      (Arbitrary (..), CoArbitrary (..),
                                       arbitraryBoundedIntegral,
                                       coarbitraryIntegral, shrinkIntegral)

import CLaSH.Class.BitPack            (BitPack (..))
import CLaSH.Class.Num                (ExtendingNum (..), SaturatingNum (..),
                                       SaturationMode (..))
import CLaSH.Class.Resize             (Resize (..))
import CLaSH.Prelude.BitIndex         ((!), msb, replaceBit, split)
import CLaSH.Prelude.BitReduction     (reduceAnd, reduceOr)
import CLaSH.Promoted.Ord             (Max)
import CLaSH.Sized.Internal.BitVector (BitVector (..), Bit, (++#), high, low)
import qualified CLaSH.Sized.Internal.BitVector as BV

-- | Arbitrary-width signed integer represented by @n@ bits, including the sign
-- bit.
--
-- Uses standard 2-complements representation. Meaning that, given @n@ bits,
-- a 'Signed' @n@ number has a range of: [-(2^(@n@-1)) .. 2^(@n@-1)-1]
--
-- __NB__: The 'Num' operators perform @wrap-around@ on overflow. If you want
-- saturation on overflow, check out the 'SaturatingNum' class.
--
-- >>>  maxBound :: Signed 3
-- 3
-- >>> minBound :: Signed 3
-- -4
-- >>> read (show (minBound :: Signed 3)) :: Signed 3
-- -4
-- >>> 1 + 2 :: Signed 3
-- 3
-- >>> 2 + 3 :: Signed 3
-- -3
-- >>> (-2) + (-3) :: Signed 3
-- 3
-- >>> 2 * 3 :: Signed 4
-- 6
-- >>> 2 * 4 :: Signed 4
-- -8
-- >>> (2 :: Signed 3) `times` (4 :: Signed 4) :: Signed 7
-- 8
-- >>> (2 :: Signed 3) `plus` (3 :: Signed 3) :: Signed 4
-- 5
-- >>> (-2 :: Signed 3) `plus` (-3 :: Signed 3) :: Signed 4
-- -5
-- >>> satPlus SatSymmetric 2 3 :: Signed 3
-- 3
-- >>> satPlus SatSymmetric (-2) (-3) :: Signed 3
-- -3
newtype Signed (n :: Nat) =
    -- | The constructor, 'S', and the field, 'unsafeToInteger', are not
    -- synthesisable.
    S { unsafeToInteger :: Integer}
  deriving (Data)

{-# NOINLINE size# #-}
size# :: KnownNat n => Signed n -> Int
size# bv = fromInteger (natVal bv)

instance NFData (Signed n) where
  rnf (S i) = rnf i `seq` ()
  {-# NOINLINE rnf #-}
  -- NOINLINE is needed so that CLaSH doesn't trip on the "Signed ~# Integer"
  -- coercion

instance Show (Signed n) where
  show (S i) = show i
  {-# NOINLINE show #-}

-- | None of the 'Read' class' methods are synthesisable.
instance KnownNat (2^(n-1)) => Read (Signed n) where
  readPrec = fromIntegral <$> (readPrec :: ReadPrec Int)

instance (KnownNat (2^n), KnownNat (2^(n-1))) => BitPack (Signed n) where
  type BitSize (Signed n) = n
  pack   = pack#
  unpack = unpack#

{-# NOINLINE pack# #-}
pack# :: forall n . KnownNat (2^n) => Signed n -> BitVector n
pack# (S i) = BV (i `mod` maxI)
  where
    maxI = natVal (Proxy :: Proxy (2^n))

{-# NOINLINE unpack# #-}
unpack# :: KnownNat (2^(n-1)) => BitVector n -> Signed n
unpack# (BV i) = fromInteger_INLINE i

instance Eq (Signed n) where
  (==) = eq#
  (/=) = neq#

{-# NOINLINE eq# #-}
eq# :: Signed n -> Signed n -> Bool
eq# (S v1) (S v2) = v1 == v2

{-# NOINLINE neq# #-}
neq# :: Signed n -> Signed n -> Bool
neq# (S v1) (S v2) = v1 /= v2

instance Ord (Signed n) where
  (<)  = lt#
  (>=) = ge#
  (>)  = gt#
  (<=) = le#

lt#,ge#,gt#,le# :: Signed n -> Signed n -> Bool
{-# NOINLINE lt# #-}
lt# (S n) (S m) = n < m
{-# NOINLINE ge# #-}
ge# (S n) (S m) = n >= m
{-# NOINLINE gt# #-}
gt# (S n) (S m) = n > m
{-# NOINLINE le# #-}
le# (S n) (S m) = n <= m

-- | The functions: 'enumFrom', 'enumFromThen', 'enumFromTo', and
-- 'enumFromThenTo', are not synthesisable.
instance KnownNat (2^(n-1)) => Enum (Signed n) where
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
enumFrom#       :: KnownNat (2^(n-1)) => Signed n -> [Signed n]
enumFromThen#   :: KnownNat (2^(n-1)) => Signed n -> Signed n -> [Signed n]
enumFromTo#     :: KnownNat (2^(n-1)) => Signed n -> Signed n -> [Signed n]
enumFromThenTo# :: KnownNat (2^(n-1)) => Signed n -> Signed n -> Signed n -> [Signed n]
enumFrom# x             = map toEnum [fromEnum x ..]
enumFromThen# x y       = map toEnum [fromEnum x, fromEnum y ..]
enumFromTo# x y         = map toEnum [fromEnum x .. fromEnum y]
enumFromThenTo# x1 x2 y = map toEnum [fromEnum x1, fromEnum x2 .. fromEnum y]


instance KnownNat n => Bounded (Signed n) where
  minBound = minBound#
  maxBound = maxBound#

minBound#,maxBound# :: KnownNat n => Signed n
{-# NOINLINE minBound# #-}
minBound# = let res = S $ negate $ 2 ^ (natVal res - 1) in res
{-# NOINLINE maxBound# #-}
maxBound# = let res = S $ 2 ^ (natVal res - 1) - 1 in res

-- | Operators do @wrap-around@ on overflow
instance KnownNat (2^(n-1)) => Num (Signed n) where
  (+)         = (+#)
  (-)         = (-#)
  (*)         = (*#)
  negate      = negate#
  abs         = abs#
  signum s    = if s < 0 then (-1) else
                   if s > 0 then 1 else 0
  fromInteger = fromInteger#

(+#), (-#), (*#) :: forall n . KnownNat (2^(n-1)) => Signed n -> Signed n -> Signed n
{-# NOINLINE (+#) #-}
(S a) +# (S b) = let m  = natVal (Proxy :: Proxy (2^(n-1)))
                     z  = a + b
                 in  if z >= m then S (z - 2*m) else
                        if z < negate m then S (z + 2*m) else S z

{-# NOINLINE (-#) #-}
(S a) -# (S b) = let m  = natVal (Proxy :: Proxy (2^(n-1)))
                     z  = a - b
                 in  if z < negate m then S (z + 2*m) else
                        if z >= m then S (z - 2*m) else S z

{-# NOINLINE (*#) #-}
(S a) *# (S b) = fromInteger_INLINE (a * b)

negate#,abs# :: forall n . KnownNat (2^(n-1)) => Signed n -> Signed n
{-# NOINLINE negate# #-}
negate# (S n) = let m = natVal (Proxy :: Proxy (2^(n-1)))
                    z = negate n
                in  if z == m then S n else S z

{-# NOINLINE abs# #-}
abs# (S n) = let m = natVal (Proxy :: Proxy (2^(n-1)))
                 z = abs n
             in  if z == m then S n else S z

{-# NOINLINE fromInteger# #-}
fromInteger# :: KnownNat (2^(n-1)) => Integer -> Signed (n :: Nat)
fromInteger# = fromInteger_INLINE

{-# INLINE fromInteger_INLINE #-}
fromInteger_INLINE :: forall n . KnownNat (2^(n-1)) => Integer -> Signed n
fromInteger_INLINE i = mask `seq` S res
  where
    mask = natVal (Proxy :: Proxy (2^(n-1)))
    res  = case divMod i mask of
             (s,i') | even s    -> i'
                    | otherwise -> i' - mask

instance ExtendingNum (Signed m) (Signed n) where
  type AResult (Signed m) (Signed n) = Signed (Max m n + 1)
  plus  = plus#
  minus = minus#
  type MResult (Signed m) (Signed n) = Signed (m + n)
  times = times#

plus#, minus# :: Signed m -> Signed n -> Signed (Max m n + 1)
{-# NOINLINE plus# #-}
plus# (S a) (S b) = S (a + b)

{-# NOINLINE minus# #-}
minus# (S a) (S b) = S (a - b)

{-# NOINLINE times# #-}
times# :: Signed m -> Signed n -> Signed (m + n)
times# (S a) (S b) = S (a * b)

instance KnownNat (2^(n-1)) => Real (Signed n) where
  toRational = toRational . toInteger#

instance KnownNat (2^(n-1)) => Integral (Signed n) where
  quot        = quot#
  rem         = rem#
  div         = div#
  mod         = mod#
  quotRem n d = (n `quot#` d,n `rem#` d)
  divMod  n d = (n `div#`  d,n `mod#` d)
  toInteger   = toInteger#

quot#,rem# :: Signed n -> Signed n -> Signed n
{-# NOINLINE quot# #-}
quot# (S a) (S b) = S (a `quot` b)
{-# NOINLINE rem# #-}
rem# (S a) (S b) = S (a `rem` b)

div#,mod# :: Signed n -> Signed n -> Signed n
{-# NOINLINE div# #-}
div# (S a) (S b) = S (a `div` b)
{-# NOINLINE mod# #-}
mod# (S a) (S b) = S (a `mod` b)

{-# NOINLINE toInteger# #-}
toInteger# :: Signed n -> Integer
toInteger# (S n) = n

instance (KnownNat n, KnownNat (2^n), KnownNat (2^(n-1)), KnownNat (n + 1), KnownNat (n + 2)) => Bits (Signed n) where
  (.&.)             = and#
  (.|.)             = or#
  xor               = xor#
  complement        = complement#
  zeroBits          = 0
  bit i             = replaceBit i high 0
  setBit v i        = replaceBit i high v
  clearBit v i      = replaceBit i low  v
  complementBit v i = replaceBit i (BV.complement# (v ! i)) v
  testBit v i       = v ! i == 1
  bitSizeMaybe v    = Just (size# v)
  bitSize           = size#
  isSigned _        = True
  shiftL v i        = shiftL# v i
  shiftR v i        = shiftR# v i
  rotateL v i       = rotateL# v i
  rotateR v i       = rotateR# v i
  popCount s        = popCount (pack# s)

and#,or#,xor# :: KnownNat (2^(n-1)) => Signed n -> Signed n -> Signed n
{-# NOINLINE and# #-}
and# (S a) (S b) = fromInteger_INLINE (a .&. b)
{-# NOINLINE or# #-}
or# (S a) (S b)  = fromInteger_INLINE (a .|. b)
{-# NOINLINE xor# #-}
xor# (S a) (S b) = fromInteger_INLINE (xor a b)

{-# NOINLINE complement# #-}
complement# :: KnownNat (2^(n-1)) => Signed n -> Signed n
complement# (S a) = fromInteger_INLINE (complement a)

shiftL#,shiftR#,rotateL#,rotateR# :: (KnownNat n, KnownNat (2^(n-1))) => Signed n -> Int -> Signed n
{-# NOINLINE shiftL# #-}
shiftL# _ b | b < 0  = error "'shiftL undefined for negative numbers"
shiftL# (S n) b      = fromInteger_INLINE (shiftL n b)
{-# NOINLINE shiftR# #-}
shiftR# _ b | b < 0  = error "'shiftR undefined for negative numbers"
shiftR# (S n) b      = fromInteger_INLINE (shiftR n b)
{-# NOINLINE rotateL# #-}
rotateL# _ b | b < 0 = error "'shiftL undefined for negative numbers"
rotateL# s@(S n) b   = fromInteger_INLINE (l .|. r)
  where
    l    = shiftL n b'
    r    = shiftR n b'' .&. mask
    mask = 2 ^ b' - 1

    b'   = b `mod` sz
    b''  = sz - b'
    sz   = fromInteger (natVal s)

{-# NOINLINE rotateR# #-}
rotateR# _ b | b < 0 = error "'shiftR undefined for negative numbers"
rotateR# s@(S n) b   = fromInteger_INLINE (l .|. r)
  where
    l    = shiftR n b' .&. mask
    r    = shiftL n b''
    mask = 2 ^ b'' - 1

    b'  = b `mod` sz
    b'' = sz - b'
    sz  = fromInteger (natVal s)

instance (KnownNat n, KnownNat (2^(n-1)), KnownNat (2^n), KnownNat (n + 1), KnownNat (n + 2)) => FiniteBits (Signed n) where
  finiteBitSize        = size#
  countLeadingZeros  s = countLeadingZeros  (pack# s)
  countTrailingZeros s = countTrailingZeros (pack# s)

data ResizeSigned1 (n :: Nat) (f :: TyFun Nat Constraint) :: *
type instance Apply (ResizeSigned1 n) m = (KnownNat n, KnownNat (2^(m-1)))
data ResizeSigned (f :: TyFun Nat (TyFun Nat Constraint -> *)) :: *
type instance Apply ResizeSigned n = ResizeSigned1 n

data ExtendSigned1 (n :: Nat) (f :: TyFun Nat Constraint) :: *
type instance Apply (ExtendSigned1 n) m = (KnownNat n, KnownNat (2^n), KnownNat (2^(n-1)), KnownNat (2^m), KnownNat (2^((m+n)-1)))
data ExtendSigned (f :: TyFun Nat (TyFun Nat Constraint -> *)) :: *
type instance Apply ExtendSigned n = ExtendSigned1 n

data TruncateSigned (f :: TyFun Nat Constraint) :: *
type instance Apply TruncateSigned m = KnownNat (2^(m-1))

instance Resize Signed where
  type ResizeC Signed = ResizeSigned
  resize       = resize#
  type ExtendC Signed = ExtendSigned
  extend       = resize#
  zeroExtend s = unpack# (0 ++# pack s)
  signExtend   = resize#
  type TruncateC Signed = FlipSym1 (ConstSym1 TruncateSigned)
  truncateB    = truncateB#

{-# NOINLINE resize# #-}
resize# :: forall m n . (KnownNat n, KnownNat (2^(m-1))) => Signed n -> Signed m
resize# s@(S i) | n' <= m'  = extended
                | otherwise = truncated
  where
    n  = fromInteger (natVal s)
    n' = shiftL 1 n
    m' = shiftL mask 1
    extended = S i

    mask      = natVal (Proxy :: Proxy (2^(m-1)))
    i'        = i `mod` mask
    truncated = if testBit i (n-1)
                   then S (i' - mask)
                   else S i'

{-# NOINLINE truncateB# #-}
truncateB# :: KnownNat (2^(m-1)) => Signed (m + n) -> Signed m
truncateB# (S n) = fromInteger_INLINE n

instance KnownNat (2^(n-1)) => Default (Signed n) where
  def = fromInteger# 0

instance KnownNat n => Lift (Signed n) where
  lift s@(S i) = sigE [| fromInteger# i |] (decSigned (natVal s))
  {-# NOINLINE lift #-}

decSigned :: Integer -> TypeQ
decSigned n = appT (conT ''Signed) (litT $ numTyLit n)

instance (((n+1)-1)~n, (n+1) ~ (1+n), KnownNat (2 ^ (1 + n)), KnownNat (2 ^ (n + n)), KnownNat n, KnownNat (2^n), KnownNat (2^(n-1)), KnownNat (n-1), KnownNat (n+1), KnownNat (2^(n + n - 1))) =>
  SaturatingNum (Signed n) where
  satPlus SatWrap a b = a +# b
  satPlus w a b = case msb r `xor` msb r' of
                     0 -> unpack# r'
                     _ -> case msb a .&. msb b of
                            1 -> case w of
                                   SatBound     -> minBound#
                                   SatSymmetric -> minBoundSym#
                                   _            -> fromInteger# 0
                            _ -> case w of
                                   SatZero -> fromInteger# 0
                                   _       -> maxBound#
    where
      r      = plus# a b
      (_,r') = split r

  satMin SatWrap a b = a -# b
  satMin w a b = case msb r `xor` msb r' of
                     0 -> unpack# r'
                     _ -> case msb a ++# msb b of
                            2 -> case w of
                                   SatBound     -> minBound#
                                   SatSymmetric -> minBoundSym#
                                   _            -> fromInteger# 0
                            _ -> case w of
                                   SatZero -> fromInteger# 0
                                   _       -> maxBound#
    where
      r      = minus# a b
      (_,r') = split r


  satMult SatWrap a b = a *# b
  satMult w a b = case overflow of
                     1 -> unpack# rR
                     _ -> case msb rL of
                            0 -> case w of
                                   SatZero -> fromInteger# 0
                                   _       -> maxBound#
                            _ -> case w of
                                   SatBound     -> minBound#
                                   SatSymmetric -> minBoundSym#
                                   _            -> fromInteger# 0
    where
      overflow = complement (reduceOr (msb rR ++# pack rL)) .|.
                            reduceAnd (msb rR ++# pack rL)
      r        = times# a b
      (rL,rR)  = split r

minBoundSym# :: (KnownNat n, KnownNat (2^(n-1))) => Signed n
minBoundSym# = minBound# +# fromInteger# 1

instance (KnownNat n, KnownNat (2^(n-1))) => Arbitrary (Signed n) where
  arbitrary = arbitraryBoundedIntegral
  shrink    = shrinkSizedSigned

shrinkSizedSigned :: (KnownNat n, Integral (p n)) => p n -> [p n]
shrinkSizedSigned x | natVal x < 2 = case toInteger x of
                                       0 -> []
                                       _ -> [0]
                    -- 'shrinkIntegral' uses "`quot` 2", which for sized types
                    -- less than 2 bits wide results in a division by zero.
                    --
                    -- See: https://github.com/clash-lang/clash-compiler/issues/153
                    | otherwise    = shrinkIntegral x
{-# INLINE shrinkSizedSigned #-}

instance KnownNat (2^(n-1)) => CoArbitrary (Signed n) where
  coarbitrary = coarbitraryIntegral

type instance Index   (Signed n) = Int
type instance IxValue (Signed n) = Bit
instance (KnownNat n, KnownNat (2^n), KnownNat (2 ^ (n - 1))) => Ixed (Signed n) where
  ix i f s = unpack# <$> BV.replaceBit# (pack# s) i
                     <$> f (BV.index# (pack# s) i)
