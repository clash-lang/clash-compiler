{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE Unsafe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Copyright  :  (C) 2013-2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
module CLaSH.Sized.Internal.BitVector
  ( -- * Datatypes
    BitVector (..)
  , Bit
    -- * Accessors
    -- ** Length information
  , size#
  , maxIndex#
    -- * Construction
    -- ** Initialisation
  , high
  , low
  , bLit
    -- ** Concatenation
  , (++#)
    -- * Reduction
  , reduceAnd#
  , reduceOr#
  , reduceXor#
    -- * Indexing
  , index#
  , replaceBit#
  , setSlice#
  , slice#
  , split#
  , msb#
  , lsb#
    -- * Type classes
    -- ** Eq
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
  , popCount#
    -- ** Resize
  , resize#
  )
where

import Control.Lens               (Index, Ixed (..), IxValue)
import Data.Bits                  (Bits (..), FiniteBits (..))
import Data.Char                  (digitToInt)
import Data.Default               (Default (..))
import Data.Maybe                 (listToMaybe)
import GHC.Integer                (smallInteger)
import GHC.Prim                   (dataToTag#)
import GHC.TypeLits               (KnownNat, Nat, type (+), type (-), natVal)
import Language.Haskell.TH        (Q, TExp, TypeQ, appT, conT, litT, numTyLit, sigE)
import Language.Haskell.TH.Syntax (Lift(..))
import Numeric                    (readInt)
import Test.QuickCheck.Arbitrary  (Arbitrary (..), CoArbitrary (..),
                                   arbitrarySizedBoundedIntegral,
                                   coarbitraryIntegral, shrinkIntegral)

import CLaSH.Class.Num            (ExtendingNum (..), SaturatingNum (..),
                                   SaturationMode (..))
import CLaSH.Class.Resize         (Resize (..))
import CLaSH.Promoted.Nat         (SNat, snatToInteger)
import CLaSH.Promoted.Ord         (Max)

{- $setup
>>> :set -XTemplateHaskell
>>> :set -XBinaryLiterals
-}

-- * Type definitions

-- | A vector of bits.
--
-- * Bit indices are descending
-- * 'Num' instance performs /unsigned/ arithmetic.
newtype BitVector (n :: Nat) =
    -- | The constructor, 'BV', and  the field, 'unsafeToInteger', are not
    -- synthesisable.
    BV { unsafeToInteger :: Integer}

-- | 'Bit': a 'BitVector' of length 1
type Bit = BitVector 1

-- * Instances
instance KnownNat n => Show (BitVector n) where
  show bv@(BV i) = reverse . underScore . reverse $ showBV (natVal bv) i []
    where
      showBV 0 _ s = s
      showBV n v s = let (a,b) = divMod v 2
                     in  case b of
                           1 -> showBV (n - 1) a ('1':s)
                           _ -> showBV (n - 1) a ('0':s)

      underScore xs = case splitAt 5 xs of
                        ([a,b,c,d,e],rest) -> [a,b,c,d,'_'] ++ underScore (e:rest)
                        (rest,_)               -> rest

-- | Create a binary literal
--
-- >>> $$(bLit "1001") :: BitVector 4
-- 1001
-- >>> $$(bLit "1001") :: BitVector 3
-- 001
--
-- __NB__: You can also just write:
--
-- >>> 0b1001 :: BitVector 4
-- 1001
--
-- The advantage of 'bLit' is that you can use computations to create the
-- string literal:
--
-- >>> import qualified Data.List as List
-- >>> $$(bLit (List.replicate 4 '1')) :: BitVector 4
-- 1111
bLit :: KnownNat n => String -> Q (TExp (BitVector n))
bLit s = [|| fromInteger# i' ||]
  where
    i :: Maybe Integer
    i = fmap fst . listToMaybe . (readInt 2 (`elem` "01") digitToInt) $ filter (/= '_') s

    i' :: Integer
    i' = case i of
           Just j -> j
           _      -> error "Failed to parse: " s

instance Eq (BitVector n) where
  (==) = eq#
  (/=) = neq#

{-# NOINLINE eq# #-}
eq# :: BitVector n -> BitVector n -> Bool
eq# (BV v1) (BV v2) = v1 == v2

{-# NOINLINE neq# #-}
neq# :: BitVector n -> BitVector n -> Bool
neq# (BV v1) (BV v2) = v1 /= v2

instance Ord (BitVector n) where
  (<)  = lt#
  (>=) = ge#
  (>)  = gt#
  (<=) = le#

lt#,ge#,gt#,le# :: BitVector n -> BitVector n -> Bool
{-# NOINLINE lt# #-}
lt# (BV n) (BV m) = n < m
{-# NOINLINE ge# #-}
ge# (BV n) (BV m) = n >= m
{-# NOINLINE gt# #-}
gt# (BV n) (BV m) = n > m
{-# NOINLINE le# #-}
le# (BV n) (BV m) = n <= m

-- | The functions: 'enumFrom', 'enumFromThen', 'enumFromTo', and
-- 'enumFromThenTo', are not synthesisable.
instance KnownNat n => Enum (BitVector n) where
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
enumFrom#       :: KnownNat n => BitVector n -> [BitVector n]
enumFromThen#   :: KnownNat n => BitVector n -> BitVector n -> [BitVector n]
enumFromTo#     :: KnownNat n => BitVector n -> BitVector n -> [BitVector n]
enumFromThenTo# :: KnownNat n => BitVector n -> BitVector n -> BitVector n
                -> [BitVector n]
enumFrom# x             = map toEnum [fromEnum x ..]
enumFromThen# x y       = map toEnum [fromEnum x, fromEnum y ..]
enumFromTo# x y         = map toEnum [fromEnum x .. fromEnum y]
enumFromThenTo# x1 x2 y = map toEnum [fromEnum x1, fromEnum x2 .. fromEnum y]

instance KnownNat n => Bounded (BitVector n) where
  minBound = minBound#
  maxBound = maxBound#

{-# NOINLINE minBound# #-}
minBound# :: BitVector n
minBound# = BV 0

{-# NOINLINE maxBound# #-}
maxBound# :: KnownNat n => BitVector n
maxBound# = let res = BV ((2 ^ natVal res) - 1) in res

instance KnownNat n => Num (BitVector n) where
  (+)         = (+#)
  (-)         = (-#)
  (*)         = (*#)
  negate      = negate#
  abs         = id
  signum bv   = resize# (reduceOr# bv)
  fromInteger = fromInteger#

(+#),(-#),(*#) :: KnownNat n => BitVector n -> BitVector n -> BitVector n
{-# NOINLINE (+#) #-}
(+#) (BV i) (BV j) = fromInteger_INLINE (i + j)

{-# NOINLINE (-#) #-}
(-#) (BV i) (BV j) = fromInteger_INLINE (i - j)

{-# NOINLINE (*#) #-}
(*#) (BV i) (BV j) = fromInteger_INLINE (i * j)

{-# NOINLINE negate# #-}
negate# :: KnownNat n => BitVector n -> BitVector n
negate# bv@(BV i) = BV (sz - i)
  where
    sz = 2 ^ natVal bv

{-# NOINLINE fromInteger# #-}
fromInteger# :: KnownNat n => Integer -> BitVector n
fromInteger# = fromInteger_INLINE

{-# INLINE fromInteger_INLINE #-}
fromInteger_INLINE :: KnownNat n => Integer -> BitVector n
fromInteger_INLINE i = let res = BV (i `mod` (2 ^ natVal res)) in res

instance (KnownNat (Max m n + 1), KnownNat (m + n)) =>
  ExtendingNum (BitVector m) (BitVector n) where
  type AResult (BitVector m) (BitVector n) = BitVector (Max m n + 1)
  plus  = plus#
  minus = minus#
  type MResult (BitVector m) (BitVector n) = BitVector (m + n)
  times = times#

plus#, minus# :: KnownNat (Max m n + 1) => BitVector m -> BitVector n
              -> BitVector (Max m n + 1)
{-# NOINLINE plus# #-}
plus# (BV a) (BV b) = fromInteger_INLINE (a + b)

{-# NOINLINE minus# #-}
minus# (BV a) (BV b) = fromInteger_INLINE (a - b)

{-# NOINLINE times# #-}
times# :: KnownNat (m + n) => BitVector m -> BitVector n -> BitVector (m + n)
times# (BV a) (BV b) = fromInteger_INLINE (a * b)

instance KnownNat n => Real (BitVector n) where
  toRational = toRational . toInteger#

instance KnownNat n => Integral (BitVector n) where
  quot        = quot#
  rem         = rem#
  div         = quot#
  mod         = rem#
  quotRem n d = (n `quot#` d,n `rem#` d)
  divMod  n d = (n `quot#` d,n `rem#` d)
  toInteger   = toInteger#

quot#,rem# :: BitVector n -> BitVector n -> BitVector n
{-# NOINLINE quot# #-}
quot# (BV i) (BV j) = BV (i `quot` j)
{-# NOINLINE rem# #-}
rem# (BV i) (BV j) = BV (i `rem` j)

{-# NOINLINE toInteger# #-}
toInteger# :: BitVector n -> Integer
toInteger# (BV i) = i

instance KnownNat n => Bits (BitVector n) where
  (.&.)             = and#
  (.|.)             = or#
  xor               = xor#
  complement        = complement#
  zeroBits          = 0
  bit i             = replaceBit# 0 i high
  setBit v i        = replaceBit# v i high
  clearBit v i      = replaceBit# v i low
  complementBit v i = replaceBit# v i (complement# (index# v i))
  testBit v i       = eq# (index# v i) high
  bitSizeMaybe v    = Just (size# v)
  bitSize           = size#
  isSigned _        = False
  shiftL v i        = shiftL# v i
  shiftR v i        = shiftR# v i
  rotateL v i       = rotateL# v i
  rotateR v i       = rotateR# v i
  popCount          = popCount#

instance KnownNat n => FiniteBits (BitVector n) where
  finiteBitSize = size#

{-# NOINLINE reduceAnd# #-}
reduceAnd# :: (KnownNat n) => BitVector n -> BitVector 1
reduceAnd# bv@(BV i) = BV (smallInteger (dataToTag# check))
  where
    check = i == maxI

    sz    = natVal bv
    maxI  = (2 ^ sz) - 1

{-# NOINLINE reduceOr# #-}
reduceOr# :: BitVector n -> BitVector 1
reduceOr# (BV i) = BV (smallInteger (dataToTag# check))
  where
    check = i /= 0

{-# NOINLINE reduceXor# #-}
reduceXor# :: BitVector n -> BitVector 1
reduceXor# (BV i) = BV (toInteger (popCount i `mod` 2))

instance Default (BitVector n) where
  def = minBound#

-- * Accessors
-- ** Length information
{-# NOINLINE size# #-}
size# :: KnownNat n => BitVector n -> Int
size# bv = fromInteger (natVal bv)

{-# NOINLINE maxIndex# #-}
maxIndex# :: KnownNat n => BitVector n -> Int
maxIndex# bv = fromInteger (natVal bv) - 1

-- ** Indexing
{-# NOINLINE index# #-}
index# :: KnownNat n => BitVector n -> Int -> Bit
index# bv@(BV v) i
    | i >= 0 && i < sz = BV (smallInteger
                            (dataToTag#
                            (testBit v i)))
    | otherwise        = err
  where
    sz  = fromInteger (natVal bv)
    err = error $ concat [ "(!): "
                         , show i
                         , " is out of range ["
                         , show (sz - 1)
                         , "..0]"
                         ]

{-# NOINLINE msb# #-}
-- | MSB
msb# :: KnownNat n => BitVector n -> Bit
msb# bv@(BV v) = BV (smallInteger (dataToTag# (testBit v i)))
  where
    i = fromInteger (natVal bv - 1)

{-# NOINLINE lsb# #-}
-- | LSB
lsb# :: BitVector n -> Bit
lsb# (BV v) = BV (smallInteger (dataToTag# (testBit v 0)))

{-# NOINLINE slice# #-}
slice# :: BitVector (m + 1 + i) -> SNat m -> SNat n -> BitVector (m + 1 - n)
slice# (BV i) m n = BV (shiftR (i .&. mask) n')
  where
    m' = snatToInteger m
    n' = fromInteger (snatToInteger n)

    mask = 2 ^ (m' + 1) - 1

-- * Constructions
-- ** Initialisation
{-# NOINLINE high #-}
-- | logic '1'
high :: Bit
high = BV 1

{-# NOINLINE low #-}
-- | logic '0'
low :: Bit
low = BV 0

-- ** Concatenation
{-# NOINLINE (++#) #-}
-- | Concatenate two 'BitVector's
(++#) :: KnownNat m => BitVector n -> BitVector m -> BitVector (n + m)
(BV v1) ++# bv2@(BV v2) = BV (v1' + v2)
  where
    v1' = shiftL v1 (fromInteger (natVal bv2))

-- * Modifying BitVectors
{-# NOINLINE replaceBit# #-}
replaceBit# :: KnownNat n => BitVector n -> Int -> Bit -> BitVector n
replaceBit# bv@(BV v) i (BV b)
    | i >= 0 && i < sz = BV (if b == 1 then setBit v i else clearBit v i)
    | otherwise        = err
  where
    sz   = fromInteger (natVal bv)
    err  = error $ concat [ "replaceBit: "
                          , show i
                          , " is out of range ["
                          , show (sz - 1)
                          , "..0]"
                          ]

{-# NOINLINE setSlice# #-}
setSlice# :: BitVector (m + 1 + i) -> SNat m -> SNat n -> BitVector (m + 1 - n)
          -> BitVector (m + 1 + i)
setSlice# (BV i) m n (BV j) = BV ((i .&. mask) .|. j')
  where
    m' = snatToInteger m
    n' = snatToInteger n

    j'   = shiftL j (fromInteger n')
    mask = complement ((2 ^ (m' + 1) - 1) `xor` (2 ^ n' - 1))

{-# NOINLINE split# #-}
split# :: KnownNat n => BitVector (m + n) -> (BitVector m, BitVector n)
split# (BV i) = (l,r)
  where
    n    = fromInteger (natVal r)
    mask = (2 ^ n) - 1
    r    = BV (i .&. mask)
    l    = BV (i `shiftR` n)

and#, or#, xor# :: BitVector n -> BitVector n -> BitVector n
{-# NOINLINE and# #-}
and# (BV v1) (BV v2) = BV (v1 .&. v2)

{-# NOINLINE or# #-}
or# (BV v1) (BV v2) = BV (v1 .|. v2)

{-# NOINLINE xor# #-}
xor# (BV v1) (BV v2) = BV (v1 `xor` v2)

{-# NOINLINE complement# #-}
complement# :: KnownNat n => BitVector n -> BitVector n
complement# (BV v1) = fromInteger_INLINE (complement v1)

shiftL#, shiftR#, rotateL#, rotateR# :: KnownNat n => BitVector n -> Int
                                     -> BitVector n
{-# NOINLINE shiftL# #-}
shiftL# (BV v) i
  | i < 0     = error
              $ "'shiftL undefined for negative number: " ++ show i
  | otherwise = fromInteger_INLINE (shiftL v i)

{-# NOINLINE shiftR# #-}
shiftR# (BV v) i
  | i < 0     = error
              $ "'shiftR undefined for negative number: " ++ show i
  | otherwise = fromInteger_INLINE (shiftR v i)

{-# NOINLINE rotateL# #-}
rotateL# _ b | b < 0 = error "'shiftL undefined for negative numbers"
rotateL# bv@(BV n) b   = fromInteger_INLINE (l .|. r)
  where
    l    = shiftL n b'
    r    = shiftR n b''

    b'   = b `mod` sz
    b''  = sz - b'
    sz   = fromInteger (natVal bv)

{-# NOINLINE rotateR# #-}
rotateR# _ b | b < 0 = error "'shiftR undefined for negative numbers"
rotateR# bv@(BV n) b   = fromInteger_INLINE (l .|. r)
  where
    l   = shiftR n b'
    r   = shiftL n b''

    b'  = b `mod` sz
    b'' = sz - b'
    sz  = fromInteger (natVal bv)

{-# NOINLINE popCount# #-}
popCount# :: BitVector n -> Int
popCount# (BV i) = popCount i

instance Resize BitVector where
  resize     = resize#
  zeroExtend = resize#
  signExtend = resize#
  truncateB  = resize#

{-# NOINLINE resize# #-}
resize# :: KnownNat m => BitVector n -> BitVector m
resize# (BV n) = fromInteger_INLINE n

instance KnownNat n => Lift (BitVector n) where
  lift bv@(BV i) = sigE [| fromInteger# i |] (decBitVector (natVal bv))

decBitVector :: Integer -> TypeQ
decBitVector n = appT (conT ''BitVector) (litT $ numTyLit n)

instance (KnownNat n, KnownNat (n + 1), KnownNat (n + n)) =>
  SaturatingNum (BitVector n) where
  satPlus SatWrap a b = a +# b
  satPlus w a b = case msb# r of
                   0 -> resize# r
                   _ -> case w of
                          SatZero  -> minBound#
                          _        -> maxBound#
    where
      r = plus# a b

  satMin SatWrap a b = a -# b
  satMin _ a b = case msb# r of
                   0 -> resize# r
                   _ -> minBound#
    where
      r = minus# a b

  satMult SatWrap a b = a *# b
  satMult w a b = case rL of
                     0 -> rR
                     _ -> case w of
                            SatZero  -> minBound#
                            _        -> maxBound#
    where
      r       = times# a b
      (rL,rR) = split# r

instance KnownNat n => Arbitrary (BitVector n) where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink    = shrinkIntegral

instance KnownNat n => CoArbitrary (BitVector n) where
  coarbitrary = coarbitraryIntegral

type instance Index   (BitVector n) = Int
type instance IxValue (BitVector n) = Bit
instance KnownNat n => Ixed (BitVector n) where
  ix i f bv = replaceBit# bv i <$> f (index# bv i)
