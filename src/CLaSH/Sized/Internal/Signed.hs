{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

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
  , popCount#
    -- ** Resize
  , resize#
  , truncateB#
    -- ** SaturatingNum
  , minBoundSym#
  )
where

import Data.Bits                      (Bits (..), FiniteBits (..))
import Data.Default                   (Default (..))
import Data.Typeable                  (Typeable)
import GHC.TypeLits                   (KnownNat, Nat, type (+), natVal)
import Language.Haskell.TH            (TypeQ, appT, conT, litT, numTyLit, sigE)
import Language.Haskell.TH.Syntax     (Lift(..))

import CLaSH.Class.BitPack            (BitPack (..))
import CLaSH.Class.Num                (ExtendingNum (..), SaturatingNum (..),
                                       SaturationMode (..))
import CLaSH.Class.Resize             (Resize (..))
import CLaSH.Prelude.BitIndex         ((!), msb, replaceBit, split)
import CLaSH.Prelude.BitReduction     (reduceAnd, reduceOr)
import CLaSH.Promoted.Ord             (Max)
import CLaSH.Sized.Internal.BitVector (BitVector (..), (++#), high, low)
import qualified CLaSH.Sized.Internal.BitVector as BV

-- | Arbitrary-width signed integer represented by @n@ bits, including the sign
-- bit.
--
-- Uses standard 2-complements representation. Meaning that, given @n@ bits,
-- a 'Signed' @n@ number has a range of: [-(2^(@n@-1)) .. 2^(@n@-1)-1]
--
-- __NB__: The 'Num' operators perform @wrap-around@ on overflow. If you want
-- saturation on overflow, check out the 'SaturatingNum' class.
newtype Signed (n :: Nat) =
    -- | The constructor, 'S', and the field, 'unsafeToInteger', are not
    -- synthesisable.
    S { unsafeToInteger :: Integer}
  deriving Typeable

{-# NOINLINE size# #-}
size# :: KnownNat n => Signed n -> Int
size# bv = fromInteger (natVal bv)

instance Show (Signed n) where
  show (S n) = show n

instance KnownNat n => BitPack (Signed n) where
  type BitSize (Signed n) = n
  pack   = pack#
  unpack = unpack#

{-# NOINLINE pack# #-}
pack# :: KnownNat n => Signed n -> BitVector n
pack# s@(S i) = BV (i `mod` maxI)
  where
    maxI = 2 ^ natVal s

{-# NOINLINE unpack# #-}
unpack# :: KnownNat n => BitVector n -> Signed n
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
enumFromTo#     :: KnownNat n => Signed n -> Signed n -> [Signed n]
enumFromThenTo# :: KnownNat n => Signed n -> Signed n -> Signed n -> [Signed n]
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
instance KnownNat n => Num (Signed n) where
  (+)         = (+#)
  (-)         = (-#)
  (*)         = (*#)
  negate      = negate#
  abs         = abs#
  signum s    = if s < 0 then (-1) else
                   if s > 0 then 1 else 0
  fromInteger = fromInteger#

(+#), (-#), (*#) :: KnownNat n => Signed n -> Signed n -> Signed n
{-# NOINLINE (+#) #-}
(S a) +# (S b) = fromInteger_INLINE (a + b)

{-# NOINLINE (-#) #-}
(S a) -# (S b) = fromInteger_INLINE (a - b)

{-# NOINLINE (*#) #-}
(S a) *# (S b) = fromInteger_INLINE (a * b)

negate#,abs# :: KnownNat n => Signed n -> Signed n
{-# NOINLINE negate# #-}
negate# (S n) = fromInteger_INLINE (negate n)

{-# NOINLINE abs# #-}
abs# (S n) = fromInteger_INLINE (abs n)

{-# NOINLINE fromInteger# #-}
fromInteger# :: KnownNat n => Integer -> Signed (n :: Nat)
fromInteger# = fromInteger_INLINE

{-# INLINE fromInteger_INLINE #-}
fromInteger_INLINE :: KnownNat n => Integer -> Signed n
fromInteger_INLINE i
    | n == 0    = S 0
    | otherwise = res
  where
    n   = natVal res
    sz  = 2 ^ (n - 1)
    res = case divMod i sz of
            (s,i') | even s    -> S i'
                   | otherwise -> S (i' - sz)

instance (KnownNat (1 + Max m n), KnownNat (m + n)) =>
  ExtendingNum (Signed m) (Signed n) where
  type AResult (Signed m) (Signed n) = Signed (1 + Max m n)
  plus  = plus#
  minus = minus#
  type MResult (Signed m) (Signed n) = Signed (m + n)
  times = times#

plus#, minus# :: KnownNat (1 + Max m n) => Signed m -> Signed n
              -> Signed (1 + Max m n)
{-# NOINLINE plus# #-}
plus# (S a) (S b) = fromInteger_INLINE (a + b)

{-# NOINLINE minus# #-}
minus# (S a) (S b) = fromInteger_INLINE (a - b)

{-# NOINLINE times# #-}
times# :: KnownNat (m + n) => Signed m -> Signed n -> Signed (m + n)
times# (S a) (S b) = fromInteger_INLINE (a * b)

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

quot#,rem#,div#,mod# :: KnownNat n => Signed n -> Signed n -> Signed n
{-# NOINLINE quot# #-}
quot# (S a) (S b) = S (a `quot` b)
{-# NOINLINE rem# #-}
rem# (S a) (S b) = S (a `rem` b)
{-# NOINLINE div# #-}
div# (S a) (S b) = S (a `div` b)
{-# NOINLINE mod# #-}
mod# (S a) (S b) = S (a `mod` b)

{-# NOINLINE toInteger# #-}
toInteger# :: Signed n -> Integer
toInteger# (S n) = n

instance KnownNat n => Bits (Signed n) where
  (.&.)             = and#
  (.|.)             = or#
  xor               = xor#
  complement        = complement#
  zeroBits          = 0
  bit i             = replaceBit 0 i high
  setBit v i        = replaceBit v i high
  clearBit v i      = replaceBit v i low
  complementBit v i = replaceBit v i (BV.complement# (v ! i))
  testBit v i       = v ! i == 1
  bitSizeMaybe v    = Just (size# v)
  bitSize           = size#
  isSigned _        = True
  shiftL v i        = shiftL# v i
  shiftR v i        = shiftR# v i
  rotateL v i       = rotateL# v i
  rotateR v i       = rotateR# v i
  popCount          = popCount#

and#,or#,xor# :: KnownNat n => Signed n -> Signed n -> Signed n
{-# NOINLINE and# #-}
and# (S a) (S b) = fromInteger_INLINE (a .&. b)
{-# NOINLINE or# #-}
or# (S a) (S b)  = fromInteger_INLINE (a .|. b)
{-# NOINLINE xor# #-}
xor# (S a) (S b) = fromInteger_INLINE (xor a b)

{-# NOINLINE complement# #-}
complement# :: KnownNat n => Signed n -> Signed n
complement# = unpack# . complement . pack#

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

{-# NOINLINE popCount# #-}
popCount# :: KnownNat n => Signed n -> Int
popCount# s@(S i) = popCount i'
  where
    maxI = 2 ^ natVal s
    i'   = i `mod` maxI

instance KnownNat n => FiniteBits (Signed n) where
  finiteBitSize = size#

instance Resize Signed where
  resize       = resize#
  extend       = resize#
  zeroExtend s = unpack# (0 ++# pack s)
  signExtend   = resize#
  truncateB    = truncateB#

{-# NOINLINE resize# #-}
resize# :: (KnownNat n, KnownNat m) => Signed n -> Signed m
resize# s@(S i) | n <= m    = extended
                | otherwise = truncated
  where
    n = fromInteger (natVal s)
    m = fromInteger (natVal extended)

    extended = fromInteger_INLINE i

    mask      = (2 ^ (m - 1)) - 1
    sign      = 2 ^ (m - 1)
    i'        = i .&. mask
    truncated = if testBit i (n - 1)
                   then fromInteger_INLINE (i' .|. sign)
                   else fromInteger_INLINE i'

{-# NOINLINE truncateB# #-}
truncateB# :: KnownNat m => Signed (n + m) -> Signed m
truncateB# (S n) = fromInteger_INLINE n

instance KnownNat n => Default (Signed n) where
  def = fromInteger# 0

instance KnownNat n => Lift (Signed n) where
  lift s@(S i) = sigE [| fromInteger# i |] (decSigned (natVal s))

decSigned :: Integer -> TypeQ
decSigned n = appT (conT ''Signed) (litT $ numTyLit n)

instance (KnownNat n, KnownNat (1 + n), KnownNat (n + n)) =>
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

minBoundSym# :: KnownNat n => Signed n
minBoundSym# = minBound# +# fromInteger# 1
