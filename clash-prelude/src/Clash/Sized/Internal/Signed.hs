{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2016     , Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE Unsafe #-}

{-# OPTIONS_HADDOCK show-extensions not-home #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Clash.Sized.Internal.Signed
  ( -- * Datatypes
    Signed (..)
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

import Prelude hiding                 (odd, even)

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
import Test.QuickCheck.Arbitrary      (Arbitrary (..), CoArbitrary (..),
                                       arbitraryBoundedIntegral,
                                       coarbitraryIntegral, shrinkIntegral)

import Clash.Class.BitPack            (BitPack (..), packXWith)
import Clash.Class.Num                (ExtendingNum (..), SaturatingNum (..),
                                       SaturationMode (..))
import Clash.Class.Parity             (Parity (..))
import Clash.Class.Resize             (Resize (..))
import Clash.Prelude.BitIndex         ((!), msb, replaceBit, split)
import Clash.Prelude.BitReduction     (reduceAnd, reduceOr)
import Clash.Sized.Internal.BitVector (BitVector (BV), Bit, (++#), high, low, undefError)
import qualified Clash.Sized.Internal.BitVector as BV
import Clash.XException
  (ShowX (..), NFDataX (..), errorX, showsPrecXWith, rwhnfX)

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
-- >>> (2 :: Signed 3) `mul` (4 :: Signed 4) :: Signed 7
-- 8
-- >>> (2 :: Signed 3) `add` (3 :: Signed 3) :: Signed 4
-- 5
-- >>> (-2 :: Signed 3) `add` (-3 :: Signed 3) :: Signed 4
-- -5
-- >>> satAdd SatSymmetric 2 3 :: Signed 3
-- 3
-- >>> satAdd SatSymmetric (-2) (-3) :: Signed 3
-- -3
newtype Signed (n :: Nat) =
    -- | The constructor, 'S', and the field, 'unsafeToInteger', are not
    -- synthesizable.
    S { unsafeToInteger :: Integer}
  deriving (Data, Generic)

instance NFDataX (Signed n) where
  deepErrorX = errorX
  rnfX = rwhnfX

{-# NOINLINE size# #-}
size# :: KnownNat n => Signed n -> Int
size# bv = fromInteger (natVal bv)

instance NFData (Signed n) where
  rnf (S i) = rnf i `seq` ()
  {-# NOINLINE rnf #-}
  -- NOINLINE is needed so that Clash doesn't trip on the "Signed ~# Integer"
  -- coercion

instance Show (Signed n) where
  show (S i) = show i
  {-# NOINLINE show #-}

instance ShowX (Signed n) where
  showsPrecX = showsPrecXWith showsPrec

-- | None of the 'Read' class' methods are synthesizable.
instance KnownNat n => Read (Signed n) where
  readPrec = fromIntegral <$> (readPrec :: ReadPrec Integer)

instance KnownNat n => BitPack (Signed n) where
  type BitSize (Signed n) = n
  pack   = packXWith pack#
  unpack = unpack#

{-# NOINLINE pack# #-}
pack# :: forall n . KnownNat n => Signed n -> BitVector n
pack# (S i) = let m = 1 `shiftL` fromInteger (natVal (Proxy @n))
              in  if i < 0 then BV 0 (m + i) else BV 0 i

{-# NOINLINE unpack# #-}
unpack# :: forall n . KnownNat n => BitVector n -> Signed n
unpack# (BV 0 i) =
  let m = 1 `shiftL` fromInteger (natVal (Proxy @n) - 1)
  in  if i >= m then S (i-2*m) else S i
unpack# bv = undefError "Signed.unpack" [bv]

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
-- 'enumFromThenTo', are not synthesizable.
instance KnownNat n => Enum (Signed n) where
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
enumFrom#       :: KnownNat n => Signed n -> [Signed n]
enumFromThen#   :: KnownNat n => Signed n -> Signed n -> [Signed n]
enumFromTo#     :: Signed n -> Signed n -> [Signed n]
enumFromThenTo# :: Signed n -> Signed n -> Signed n -> [Signed n]
enumFrom# x             = map fromInteger_INLINE [unsafeToInteger x ..]
enumFromThen# x y       = map fromInteger_INLINE [unsafeToInteger x, unsafeToInteger y ..]
enumFromTo# x y         = map S [unsafeToInteger x .. unsafeToInteger y]
enumFromThenTo# x1 x2 y = map S [unsafeToInteger x1, unsafeToInteger x2 .. unsafeToInteger y]


instance KnownNat n => Bounded (Signed n) where
  minBound = minBound#
  maxBound = maxBound#

minBound#,maxBound# :: KnownNat n => Signed n
{-# NOINLINE minBound# #-}
minBound# = let res = S $ negate $ 2 ^ (natVal res - 1) in res
{-# NOINLINE maxBound# #-}
maxBound# = let res = S $ 2 ^ (natVal res - 1) - 1 in res

-- | Operators do @wrap-around@ on overflow
instance KnownNat n => Num (Signed n) where
  (+)         = (+#)
  (-)         = (-#)
  (*)         = (*#)
  negate      = negate#
  abs         = abs#
  signum s    = if s < 0 then (-1) else
                   if s > 0 then 1 else 0
  fromInteger = fromInteger#

(+#), (-#), (*#) :: forall n . KnownNat n => Signed n -> Signed n -> Signed n
{-# NOINLINE (+#) #-}
(S a) +# (S b) = let m  = 1 `shiftL` fromInteger (natVal (Proxy @n) -1)
                     z  = a + b
                 in  if z >= m then S (z - 2*m) else
                        if z < negate m then S (z + 2*m) else S z

{-# NOINLINE (-#) #-}
(S a) -# (S b) = let m  = 1 `shiftL` fromInteger (natVal (Proxy @n) -1)
                     z  = a - b
                 in  if z < negate m then S (z + 2*m) else
                        if z >= m then S (z - 2*m) else S z

{-# NOINLINE (*#) #-}
(S a) *# (S b) = fromInteger_INLINE (a * b)

negate#,abs# :: forall n . KnownNat n => Signed n -> Signed n
{-# NOINLINE negate# #-}
negate# (S n) = let m = 1 `shiftL` fromInteger (natVal (Proxy @n) -1)
                    z = negate n
                in  if z == m then S n else S z

{-# NOINLINE abs# #-}
abs# (S n) = let m = 1 `shiftL` fromInteger (natVal (Proxy @n) -1)
                 z = abs n
             in  if z == m then S n else S z

{-# NOINLINE fromInteger# #-}
fromInteger# :: KnownNat n => Integer -> Signed (n :: Nat)
fromInteger# = fromInteger_INLINE

{-# INLINE fromInteger_INLINE #-}
fromInteger_INLINE :: forall n . KnownNat n => Integer -> Signed n
fromInteger_INLINE i = if mask == 0 then S 0 else S res
  where
    mask = 1 `shiftL` fromInteger (natVal (Proxy @n) -1)
    res  = case divMod i mask of
             (s,i') | even s    -> i'
                    | otherwise -> i' - mask

instance ExtendingNum (Signed m) (Signed n) where
  type AResult (Signed m) (Signed n) = Signed (Max m n + 1)
  add  = plus#
  sub = minus#
  type MResult (Signed m) (Signed n) = Signed (m + n)
  mul = times#

plus#, minus# :: Signed m -> Signed n -> Signed (Max m n + 1)
{-# NOINLINE plus# #-}
plus# (S a) (S b) = S (a + b)

{-# NOINLINE minus# #-}
minus# (S a) (S b) = S (a - b)

{-# NOINLINE times# #-}
times# :: Signed m -> Signed n -> Signed (m + n)
times# (S a) (S b) = S (a * b)

instance KnownNat n => Real (Signed n) where
  toRational = toRational . toInteger#

instance KnownNat n => Integral (Signed n) where
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

instance KnownNat n => Parity (Signed n) where
  even = even . pack
  odd = odd . pack

instance KnownNat n => Bits (Signed n) where
  (.&.)             = and#
  (.|.)             = or#
  xor               = xor#
  complement        = complement#
  zeroBits          = 0
  bit i             = replaceBit i high 0
  setBit v i        = replaceBit i high v
  clearBit v i      = replaceBit i low  v
  complementBit v i = replaceBit i (BV.complement## (v ! i)) v
  testBit v i       = v ! i == 1
  bitSizeMaybe v    = Just (size# v)
  bitSize           = size#
  isSigned _        = True
  shiftL v i        = shiftL# v i
  shiftR v i        = shiftR# v i
  rotateL v i       = rotateL# v i
  rotateR v i       = rotateR# v i
  popCount s        = popCount (pack# s)

and#,or#,xor# :: KnownNat n => Signed n -> Signed n -> Signed n
{-# NOINLINE and# #-}
and# (S a) (S b) = fromInteger_INLINE (a .&. b)
{-# NOINLINE or# #-}
or# (S a) (S b)  = fromInteger_INLINE (a .|. b)
{-# NOINLINE xor# #-}
xor# (S a) (S b) = fromInteger_INLINE (xor a b)

{-# NOINLINE complement# #-}
complement# :: KnownNat n => Signed n -> Signed n
complement# (S a) = fromInteger_INLINE (complement a)

shiftL#,shiftR#,rotateL#,rotateR# :: KnownNat n => Signed n -> Int -> Signed n
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

instance KnownNat n => FiniteBits (Signed n) where
  finiteBitSize        = size#
  countLeadingZeros  s = countLeadingZeros  (pack# s)
  countTrailingZeros s = countTrailingZeros (pack# s)

instance Resize Signed where
  resize       = resize#
  zeroExtend s = unpack# (0 ++# pack s)
  truncateB    = truncateB#

{-# NOINLINE resize# #-}
resize# :: forall m n . (KnownNat n, KnownNat m) => Signed n -> Signed m
resize# s@(S i) | n' <= m'  = extended
                | otherwise = truncated
  where
    n  = fromInteger (natVal s)
    n' = shiftL 1 n
    m' = shiftL mask 1
    extended = S i

    mask      = 1 `shiftL` fromInteger (natVal (Proxy @m) -1)
    i'        = i `mod` mask
    truncated = if testBit i (n-1)
                   then S (i' - mask)
                   else S i'

{-# NOINLINE truncateB# #-}
truncateB# :: KnownNat m => Signed (m + n) -> Signed m
truncateB# (S n) = fromInteger_INLINE n

instance KnownNat n => Default (Signed n) where
  def = fromInteger# 0

instance KnownNat n => Lift (Signed n) where
  lift s@(S i) = sigE [| fromInteger# i |] (decSigned (natVal s))
  {-# NOINLINE lift #-}

decSigned :: Integer -> TypeQ
decSigned n = appT (conT ''Signed) (litT $ numTyLit n)

instance KnownNat n => SaturatingNum (Signed n) where
  satAdd SatWrap  a b = a +# b
  satAdd SatBound a b =
    let r      = plus# a b
        (_,r') = split r
    in  case msb r `xor` msb r' of
          0 -> unpack# r'
          _ -> case msb a .&. msb b of
            0 -> maxBound#
            _ -> minBound#
  satAdd SatZero a b =
    let r      = plus# a b
        (_,r') = split r
    in  case msb r `xor` msb r' of
          0 -> unpack# r'
          _ -> fromInteger# 0
  satAdd SatSymmetric a b =
    let r      = plus# a b
        (_,r') = split r
    in  case msb r `xor` msb r' of
          0 -> unpack# r'
          _ -> case msb a .&. msb b of
            0 -> maxBound#
            _ -> minBoundSym#

  satSub SatWrap a b = a -# b
  satSub SatBound a b =
    let r      = minus# a b
        (_,r') = split r
    in  case msb r `xor` msb r' of
          0 -> unpack# r'
          _ -> case BV.pack# (msb a) ++# BV.pack# (msb b) of
            2 -> minBound#
            _ -> maxBound#
  satSub SatZero a b =
    let r      = minus# a b
        (_,r') = split r
    in  case msb r `xor` msb r' of
          0 -> unpack# r'
          _ -> fromInteger# 0
  satSub SatSymmetric a b =
    let r      = minus# a b
        (_,r') = split r
    in  case msb r `xor` msb r' of
          0 -> unpack# r'
          _ -> case BV.pack# (msb a) ++# BV.pack# (msb b) of
            2 -> minBoundSym#
            _ -> maxBound#

  satMul SatWrap a b = a *# b
  satMul SatBound a b =
    let r        = times# a b
        (rL,rR)  = split r
        overflow = complement (reduceOr (BV.pack# (msb rR) ++# pack rL)) .|.
                              reduceAnd (BV.pack# (msb rR) ++# pack rL)
    in  case overflow of
          1 -> unpack# rR
          _ -> case msb rL of
            0 -> maxBound#
            _ -> minBound#
  satMul SatZero a b =
    let r        = times# a b
        (rL,rR)  = split r
        overflow = complement (reduceOr (BV.pack# (msb rR) ++# pack rL)) .|.
                              reduceAnd (BV.pack# (msb rR) ++# pack rL)
    in  case overflow of
          1 -> unpack# rR
          _ -> fromInteger# 0
  satMul SatSymmetric a b =
    let r        = times# a b
        (rL,rR)  = split r
        overflow = complement (reduceOr (BV.pack# (msb rR) ++# pack rL)) .|.
                              reduceAnd (BV.pack# (msb rR) ++# pack rL)
    in  case overflow of
          1 -> unpack# rR
          _ -> case msb rL of
            0 -> maxBound#
            _ -> minBoundSym#

minBoundSym# :: KnownNat n => Signed n
minBoundSym# = minBound# +# fromInteger# 1

instance KnownNat n => Arbitrary (Signed n) where
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

instance KnownNat n => CoArbitrary (Signed n) where
  coarbitrary = coarbitraryIntegral

type instance Index   (Signed n) = Int
type instance IxValue (Signed n) = Bit
instance KnownNat n => Ixed (Signed n) where
  ix i f s = unpack# <$> BV.replaceBit# (pack# s) i
                     <$> f (BV.index# (pack# s) i)
