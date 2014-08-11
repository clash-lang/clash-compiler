{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module CLaSH.Sized.Unsigned
  (Unsigned)
where

import Data.Default               (Default (..))
import Data.Typeable              (Typeable)
import GHC.TypeLits               (KnownNat, Nat, type (+), type (-), natVal)
import Language.Haskell.TH        (TypeQ, appT, conT, litT, numTyLit, sigE)
import Language.Haskell.TH.Syntax (Lift(..))

import CLaSH.Class.Bits           (Bits (..))
import CLaSH.Class.BitIndex       (BitIndex (..))
import CLaSH.Class.BitReduction   (BitReduction (..))
import CLaSH.Class.Bitwise        (Bitwise (..))
import CLaSH.Class.Num            (Add (..), Mult (..))
import CLaSH.Class.Resize         (Resize (..))
import CLaSH.Promoted.Nat         (SNat)
import CLaSH.Promoted.Ord         (Max)
import CLaSH.Sized.BitVector      (BitVector, Bit, veryUnsafeFromInteger#,
                                   veryUnsafeToInteger#)

-- | Arbitrary-width unsigned integer represented by @n@ bits
--
-- Given @n@ bits, an 'Unsigned' @n@ number has a range of: [0 .. 2^@n@-1]
--
-- __NB__: The 'Num' operators perform @wrap-around@ on overflow. If you want
-- saturation on overflow, check out the 'CLaSH.Sized.Fixed.satN2' function in
-- "CLaSH.Sized.Fixed".
newtype Unsigned (n :: Nat) = U { toBitVector :: BitVector n }
  deriving Typeable

instance Show (Unsigned n) where
  show (U bv) = show (veryUnsafeToInteger# bv)

instance KnownNat n => Bits (Unsigned n) where
  type BitSize (Unsigned n) = n
  pack   = pack#
  unpack = unpack#

pack# :: KnownNat n => Unsigned n -> BitVector n
pack# = toBitVector

unpack# :: KnownNat n => BitVector n -> Unsigned n
unpack# = U

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
enumFromTo#     :: KnownNat n => Unsigned n -> Unsigned n -> [Unsigned n]
enumFromThenTo# :: KnownNat n => Unsigned n -> Unsigned n -> Unsigned n
                -> [Unsigned n]
enumFrom# x             = map toEnum [fromEnum x ..]
enumFromThen# x y       = map toEnum [fromEnum x, fromEnum y ..]
enumFromTo# x y         = map toEnum [fromEnum x .. fromEnum y]
enumFromThenTo# x1 x2 y = map toEnum [fromEnum x1, fromEnum x2 .. fromEnum y]

instance KnownNat n => Bounded (Unsigned n) where
  minBound = minBound#
  maxBound = maxBound#

{-# NOINLINE minBound# #-}
minBound# :: Unsigned n
minBound# = U (veryUnsafeFromInteger# 0)
{-# NOINLINE maxBound# #-}
maxBound# :: KnownNat n => Unsigned n
maxBound# = U maxBound

instance KnownNat n => Num (Unsigned n) where
  (+)         = (+#)
  (-)         = (-#)
  (*)         = (*#)
  negate      = negate#
  abs         = id
  signum bv   = resize# (reduceOr# bv)
  fromInteger = fromInteger#

(+#),(-#),(*#) :: KnownNat n => Unsigned n -> Unsigned n -> Unsigned n
{-# NOINLINE (+#) #-}
(+#) (U i) (U j) = U (i + j)

{-# NOINLINE (-#) #-}
(-#) (U i) (U j) = U (i - j)

{-# NOINLINE (*#) #-}
(*#) (U i) (U j) = U (i * j)

{-# NOINLINE negate# #-}
negate# :: KnownNat n => Unsigned n -> Unsigned n
negate# (U i) = U (negate i)

{-# NOINLINE fromInteger# #-}
fromInteger# :: KnownNat n => Integer -> Unsigned n
fromInteger# i = U (fromInteger i)

instance KnownNat (Max m n + 1) => Add (Unsigned m) (Unsigned n) where
  type AResult (Unsigned m) (Unsigned n) = Unsigned (Max m n + 1)
  plus  = plus#
  minus = minus#

plus#, minus# :: KnownNat (Max m n + 1) => Unsigned m -> Unsigned n
              -> Unsigned (Max m n + 1)
{-# NOINLINE plus# #-}
plus# (U a) (U b) = U (a `plus` b)

{-# NOINLINE minus# #-}
minus# (U a) (U b) = U (a `minus` b)

instance KnownNat (m + n) => Mult (Unsigned m) (Unsigned n) where
  type MResult (Unsigned m) (Unsigned n) = Unsigned (m + n)
  mult = mult#

{-# NOINLINE mult# #-}
mult# :: KnownNat (m + n) => Unsigned m -> Unsigned n -> Unsigned (m + n)
mult# (U a) (U b) = U (a `mult` b)

instance KnownNat n => Real (Unsigned n) where
  toRational = toRational . toInteger#

instance KnownNat n => Integral (Unsigned n) where
  quot      = quot#
  rem       = rem#
  div       = div#
  mod       = mod#
  quotRem   = quotRem#
  divMod    = divMod#
  toInteger = toInteger#

quot#,rem#,div#,mod# :: KnownNat n => Unsigned n -> Unsigned n -> Unsigned n
{-# NOINLINE quot# #-}
quot# (U i) (U j) = U (i `quot` j)
{-# NOINLINE rem# #-}
rem# (U i) (U j) = U (i `rem` j)
{-# NOINLINE div# #-}
div# (U i) (U j) = U (i `div` j)
{-# NOINLINE mod# #-}
mod# (U i) (U j) = U (i `mod` j)

quotRem#,divMod# :: KnownNat n => Unsigned n -> Unsigned n
                 -> (Unsigned n, Unsigned n)
quotRem# n d = (n `quot#` d,n `rem#` d)
divMod# n d  = (n `div#` d,n `mod#` d)

{-# NOINLINE toInteger# #-}
toInteger# :: KnownNat n => Unsigned n -> Integer
toInteger# (U i) = toInteger i

instance KnownNat n => Bitwise (Unsigned n) where
  (.&.)       = and#
  (.|.)       = or#
  xor         = xor#
  complement  = complement#
  shiftL v i  = shiftL#  v (fromIntegral i)
  shiftR v i  = shiftR#  v (fromIntegral i)
  rotateL v i = rotateL# v (fromIntegral i)
  rotateR v i = rotateR# v (fromIntegral i)
  isSigned    = const False

{-# NOINLINE and# #-}
and# :: KnownNat n => Unsigned n -> Unsigned n -> Unsigned n
and# (U v1) (U v2) = U (v1 .&. v2)

{-# NOINLINE or# #-}
or# :: KnownNat n => Unsigned n -> Unsigned n -> Unsigned n
or# (U v1) (U v2) = U (v1 .|. v2)

{-# NOINLINE xor# #-}
xor# :: KnownNat n => Unsigned n -> Unsigned n -> Unsigned n
xor# (U v1) (U v2) = U (v1 `xor` v2)

{-# NOINLINE complement# #-}
complement# :: KnownNat n => Unsigned n -> Unsigned n
complement# (U v1) = U (complement v1)

{-# NOINLINE shiftL# #-}
shiftL# :: KnownNat n => Unsigned n -> Int -> Unsigned n
shiftL# (U v) i = U (shiftL v i)

{-# NOINLINE shiftR# #-}
shiftR# :: KnownNat n => Unsigned n -> Int -> Unsigned n
shiftR# (U v) i = U (shiftR v i)

{-# NOINLINE rotateL# #-}
rotateL# :: KnownNat n => Unsigned n -> Int -> Unsigned n
rotateL# (U bv) i = U (shiftL bv i)

{-# NOINLINE rotateR# #-}
rotateR# :: KnownNat n => Unsigned n -> Int -> Unsigned n
rotateR# (U bv) i = U (shiftR bv i)

instance BitIndex Unsigned where
  (!) v i    = (!#) v (fromIntegral i)
  slice      = slice#
  setBit v i = setBit# v (fromIntegral i)
  setSlice   = setSlice#
  msb        = msb#
  lsb        = lsb#

{-# NOINLINE (!#) #-}
(!#) :: KnownNat n => Unsigned n -> Int -> Bit
(U v) !# i = v ! i

{-# NOINLINE slice# #-}
slice# :: Unsigned (m + 1 + i) -> SNat m -> SNat n -> Unsigned (m + 1 - n)
slice# (U v) m n = U (slice v m n)

{-# NOINLINE setBit# #-}
setBit# :: KnownNat n => Unsigned n -> Int -> Unsigned n
setBit# (U v) i = U (setBit v i)

{-# NOINLINE setSlice# #-}
setSlice# :: Unsigned (m + 1 + i) -> SNat m -> SNat n -> Unsigned (m + 1 - n)
          -> Unsigned (m + 1 + i)
setSlice# (U v) i j (U w) = U (setSlice v i j w)

{-# NOINLINE msb# #-}
msb# :: KnownNat n => Unsigned n -> Bit
msb# (U v) = msb v

{-# NOINLINE lsb# #-}
lsb# :: KnownNat n => Unsigned n -> Bit
lsb# (U v) = lsb v

instance BitReduction Unsigned where
  reduceAnd  = reduceAnd#
  reduceOr   = reduceOr#
  reduceXor  = reduceXor#

reduceAnd#, reduceOr#, reduceXor# :: KnownNat n => Unsigned n -> Unsigned 1

{-# NOINLINE reduceAnd# #-}
reduceAnd# (U i) = U (reduceAnd i)

{-# NOINLINE reduceOr# #-}
reduceOr# (U i) = U (reduceOr i)

{-# NOINLINE reduceXor# #-}
reduceXor# (U i) = U (reduceXor i)

-- | A resize operation that zero-extends on extension, and wraps on truncation.
--
-- Increasing the size of the number extends with zeros to the left.
-- Truncating a number of length N to a length L just removes the left
-- (most significant) N-L bits.
instance Resize Unsigned where
  resize = resize#

{-# NOINLINE resize# #-}
resize# :: KnownNat m => Unsigned n -> Unsigned m
resize# (U n) = U (fromInteger (veryUnsafeToInteger# n))

instance Default (Unsigned n) where
  def = minBound#

instance KnownNat n => Lift (Unsigned n) where
  lift u@(U i) = sigE [| fromInteger# i |] (decUnsigned (natVal u))

decUnsigned :: Integer -> TypeQ
decUnsigned n = appT (conT ''Unsigned) (litT $ numTyLit n)
