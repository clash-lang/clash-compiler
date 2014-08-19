{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module CLaSH.Sized.Internal.Unsigned
  ( -- * Datatypes
    Unsigned (..)
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
  , fromInteger#
    -- ** Add
  , plus#
  , minus#
    -- ** Mult
  , mult#
    -- ** Integral
  , quot#
  , rem#
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
    -- ** SaturatingNum
  , satPlus#
  , satMin#
  , satMult#
  )
where

import Data.Bits                      (Bits (..))
import Data.Default                   (Default (..))
import Data.Typeable                  (Typeable)
import GHC.TypeLits                   (KnownNat, Nat, type (+), natVal)
import Language.Haskell.TH            (TypeQ, appT, conT, litT, numTyLit, sigE)
import Language.Haskell.TH.Syntax     (Lift(..))

import CLaSH.Class.BitConvert         (BitConvert (..))
import CLaSH.Class.Num                (Add (..), Mult (..), SaturatingNum (..),
                                       SaturationMode (..))
import CLaSH.Class.Resize             (Resize (..))
import CLaSH.Prelude.BitIndex         ((!), msb, replaceBit, split)
import CLaSH.Prelude.BitReduction     (reduceOr)
import CLaSH.Promoted.Ord             (Max)
import CLaSH.Sized.Internal.BitVector (BitVector (..), high, low)
import qualified CLaSH.Sized.Internal.BitVector as BV

-- | Arbitrary-width unsigned integer represented by @n@ bits
--
-- Given @n@ bits, an 'Unsigned' @n@ number has a range of: [0 .. 2^@n@-1]
--
-- __NB__: The 'Num' operators perform @wrap-around@ on overflow. If you want
-- saturation on overflow, check out the 'CLaSH.Sized.Fixed.satN2' function in
-- "CLaSH.Sized.Fixed".
newtype Unsigned (n :: Nat) =
    -- | The constructor, 'U', and the field, 'unsafeToBitVector', are not
    -- synthesisable.
    U { unsafeToBitVector :: BitVector n }
  deriving Typeable

{-# NOINLINE size# #-}
size# :: KnownNat n => Unsigned n -> Int
size# bv = fromInteger (natVal bv)

instance Show (Unsigned n) where
  show (U (BV i)) = show i

instance BitConvert (Unsigned n) where
  type BitSize (Unsigned n) = n
  pack   = pack#
  unpack = unpack#

{-# NOINLINE pack# #-}
pack# :: Unsigned n -> BitVector n
pack# = unsafeToBitVector

{-# NOINLINE unpack# #-}
unpack# :: BitVector n -> Unsigned n
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

-- | The functions: 'enumFrom', 'enumFromThen', 'enumFromTo', and
-- 'enumFromThenTo', are not synthesisable.
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
minBound# = U (BV 0)
{-# NOINLINE maxBound# #-}
maxBound# :: KnownNat n => Unsigned n
maxBound# = U maxBound

instance KnownNat n => Num (Unsigned n) where
  (+)         = (+#)
  (-)         = (-#)
  (*)         = (*#)
  negate      = negate#
  abs         = id
  signum bv   = resize# (unpack# (reduceOr bv))
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
  quot        = quot#
  rem         = rem#
  div         = quot#
  mod         = mod#
  quotRem n d = (n `quot#` d,n `rem#` d)
  divMod  n d = (n `quot#` d,n `mod#` d)
  toInteger   = toInteger#

quot#,rem#,mod# :: KnownNat n => Unsigned n -> Unsigned n -> Unsigned n
{-# NOINLINE quot# #-}
quot# (U i) (U j) = U (i `BV.quot#` j)
{-# NOINLINE rem# #-}
rem# (U i) (U j) = U (i `BV.rem#` j)
{-# NOINLINE mod# #-}
mod# (U i) (U j) = U (i `BV.mod#` j)

{-# NOINLINE toInteger# #-}
toInteger# :: KnownNat n => Unsigned n -> Integer
toInteger# (U i) = toInteger i

instance KnownNat n => Bits (Unsigned n) where
  (.&.)             = and#
  (.|.)             = or#
  xor               = xor#
  complement        = complement#
  zeroBits          = 0
  bit i             = replaceBit 0 i high
  setBit v i        = replaceBit v i high
  clearBit v i      = replaceBit v i low
  complementBit v i = replaceBit v i (BV.complement# (v ! i))
  testBit v i       = v ! i == high
  bitSizeMaybe v    = Just (size# v)
  bitSize           = size#
  isSigned _        = False
  shiftL v i        = shiftL# v i
  shiftR v i        = shiftR# v i
  rotateL v i       = rotateL# v i
  rotateR v i       = rotateR# v i
  popCount          = popCount#

{-# NOINLINE and# #-}
and# :: Unsigned n -> Unsigned n -> Unsigned n
and# (U v1) (U v2) = U (BV.and# v1 v2)

{-# NOINLINE or# #-}
or# :: Unsigned n -> Unsigned n -> Unsigned n
or# (U v1) (U v2) = U (BV.or# v1 v2)

{-# NOINLINE xor# #-}
xor# :: Unsigned n -> Unsigned n -> Unsigned n
xor# (U v1) (U v2) = U (BV.xor# v1 v2)

{-# NOINLINE complement# #-}
complement# :: KnownNat n => Unsigned n -> Unsigned n
complement# (U v1) = U (BV.complement# v1)

shiftL#, shiftR#, rotateL#, rotateR# :: KnownNat n => Unsigned n -> Int
                                     -> Unsigned n
{-# NOINLINE shiftL# #-}
shiftL# (U v) i = U (BV.shiftL# v i)

{-# NOINLINE shiftR# #-}
shiftR# (U v) i = U (BV.shiftR# v i)

{-# NOINLINE rotateL# #-}
rotateL# (U bv) i = U (BV.rotateL# bv i)

{-# NOINLINE rotateR# #-}
rotateR# (U bv) i = U (BV.rotateR# bv i)

{-# NOINLINE popCount# #-}
popCount# :: Unsigned n -> Int
popCount# (U bv) = BV.popCount# bv

-- | A resize operation that zero-extends on extension, and wraps on truncation.
--
-- Increasing the size of the number extends with zeros to the left.
-- Truncating a number of length N to a length L just removes the left
-- (most significant) N-L bits.
instance Resize Unsigned where
  resize = resize#

{-# NOINLINE resize# #-}
resize# :: KnownNat m => Unsigned n -> Unsigned m
resize# (U (BV i)) = U (fromInteger i)

instance Default (Unsigned n) where
  def = minBound#

instance KnownNat n => Lift (Unsigned n) where
  lift u@(U i) = sigE [| fromInteger# i |] (decUnsigned (natVal u))

decUnsigned :: Integer -> TypeQ
decUnsigned n = appT (conT ''Unsigned) (litT $ numTyLit n)

instance (KnownNat n, KnownNat (n + 1), KnownNat (n + n)) =>
  SaturatingNum (Unsigned n) where
  satPlus = satPlus#
  satMin  = satMin#
  satMult = satMult#

satPlus#, satMin# :: (KnownNat n, KnownNat (n + 1)) => SaturationMode
                  -> Unsigned n -> Unsigned n -> Unsigned n

satPlus# SatWrap a b = a +# b
satPlus# w a b = case msb r of
                   0 -> resize# r
                   _ -> case w of
                          SatZero  -> minBound#
                          _        -> maxBound#
  where
    r = plus# a b

satMin# SatWrap a b = a -# b
satMin# _ a b = case msb r of
                   0 -> resize# r
                   _ -> minBound#
  where
    r = minus# a b

satMult# :: (KnownNat n, KnownNat (n + n)) => SaturationMode -> Unsigned n
         -> Unsigned n -> Unsigned n
satMult# SatWrap a b = a *# b
satMult# w a b = case rL of
                   0 -> unpack# rR
                   _ -> case w of
                          SatZero  -> minBound#
                          _        -> maxBound#
  where
    r       = mult# a b
    (rL,rR) = split r
