{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2016     , Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# LANGUAGE Unsafe #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_HADDOCK show-extensions not-home #-}

module Clash.Sized.Internal.BitVector
  ( -- * Bit
    Bit (..)
    -- ** Construction
  , high
  , low
    -- ** Type classes
    -- *** Eq
  , eq##
  , neq##
    -- *** Ord
  , lt##
  , ge##
  , gt##
  , le##
    -- *** Num
  , fromInteger##
    -- *** Bits
  , and##
  , or##
  , xor##
  , complement##
    -- *** BitPack
  , pack#
  , unpack#
    -- * BitVector
  , BitVector (..)
    -- ** Accessors
  , size#
  , maxIndex#
    -- ** Construction
  , bLit
    -- ** Concatenation
  , (++#)
    -- ** Reduction
  , reduceAnd#
  , reduceOr#
  , reduceXor#
    -- ** Indexing
  , index#
  , replaceBit#
  , setSlice#
  , slice#
  , split#
  , msb#
  , lsb#
    -- ** Type classes
    -- **** Eq
  , eq#
  , neq#
    -- *** Ord
  , lt#
  , ge#
  , gt#
  , le#
    -- *** Enum (not synthesisable)
  , enumFrom#
  , enumFromThen#
  , enumFromTo#
  , enumFromThenTo#
    -- *** Bounded
  , minBound#
  , maxBound#
    -- *** Num
  , (+#)
  , (-#)
  , (*#)
  , negate#
  , fromInteger#
    -- *** ExtendingNum
  , plus#
  , minus#
  , times#
    -- *** Integral
  , quot#
  , rem#
  , toInteger#
    -- *** Bits
  , and#
  , or#
  , xor#
  , complement#
  , shiftL#
  , shiftR#
  , rotateL#
  , rotateR#
  , popCountBV
    -- *** FiniteBits
  , countLeadingZerosBV
  , countTrailingZerosBV
    -- *** Resize
  , resize#
    -- *** QuickCheck
  , shrinkSizedUnsigned
  )
where

import Control.DeepSeq            (NFData (..))
import Control.Lens               (Index, Ixed (..), IxValue)
import Data.Bits                  (Bits (..), FiniteBits (..))
import Data.Char                  (digitToInt)
import Data.Data                  (Data)
import Data.Default               (Default (..))
import Data.Maybe                 (listToMaybe)
import Data.Proxy                 (Proxy (..))
import GHC.Integer                (smallInteger)
import GHC.Prim                   (dataToTag#)
import GHC.TypeLits               (KnownNat, Nat, type (+), type (-), natVal)
import GHC.TypeLits.Extra         (Max)
import Language.Haskell.TH        (Q, TExp, TypeQ, appT, conT, litT, numTyLit, sigE)
import Language.Haskell.TH.Syntax (Lift(..))
import Numeric                    (readInt)
import Test.QuickCheck.Arbitrary  (Arbitrary (..), CoArbitrary (..),
                                   arbitraryBoundedIntegral,
                                   coarbitraryIntegral, shrinkIntegral)

import Clash.Class.Num            (ExtendingNum (..), SaturatingNum (..),
                                   SaturationMode (..))
import Clash.Class.Resize         (Resize (..))
import Clash.Promoted.Nat         (SNat, snatToInteger, snatToNum)
import Clash.XException           (ShowX (..), Undefined, showsPrecXWith)

import {-# SOURCE #-} qualified Clash.Sized.Vector         as V
import {-# SOURCE #-} qualified Clash.Sized.Internal.Index as I

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
  deriving (Data,Undefined)

-- * Bit

-- | Bit
newtype Bit =
  -- | The constructor, 'Bit', and  the field, 'unsafeToInteger#', are not
  -- synthesisable.
  Bit { unsafeToInteger# :: Integer}
  deriving (Data,Undefined)

-- * Constructions
-- ** Initialisation
{-# NOINLINE high #-}
-- | logic '1'
high :: Bit
high = Bit 1

{-# NOINLINE low #-}
-- | logic '0'
low :: Bit
low = Bit 0

-- ** Instances
instance NFData Bit where
  rnf (Bit i) = rnf i `seq` ()
  {-# NOINLINE rnf #-}

instance Show Bit where
  show (Bit b) =
    case b of
      0 -> "0"
      _ -> "1"

instance ShowX Bit where
  showsPrecX = showsPrecXWith showsPrec

instance Lift Bit where
  lift (Bit i) = if i == 0 then [| low |] else [| high |]
  {-# NOINLINE lift #-}

instance Eq Bit where
  (==) = eq##
  (/=) = neq##

eq## :: Bit -> Bit -> Bool
eq## (Bit b1) (Bit b2) = b1 == b2
{-# NOINLINE eq## #-}

neq## :: Bit -> Bit -> Bool
neq## (Bit b1) (Bit b2) = b1 == b2
{-# NOINLINE neq## #-}

instance Ord Bit where
  (<)  = lt##
  (<=) = le##
  (>)  = gt##
  (>=) = ge##

lt##,ge##,gt##,le## :: Bit -> Bit -> Bool
lt## (Bit n) (Bit m) = n < m
{-# NOINLINE lt## #-}
ge## (Bit n) (Bit m) = n >= m
{-# NOINLINE ge## #-}
gt## (Bit n) (Bit m) = n > m
{-# NOINLINE gt## #-}
le## (Bit n) (Bit m) = n <= m
{-# NOINLINE le## #-}

instance Enum Bit where
  toEnum     = fromInteger## . toInteger
  fromEnum b = if eq## b low then 0 else 1

instance Bounded Bit where
  minBound = low
  maxBound = high

instance Default Bit where
  def = low

instance Num Bit where
  (+)         = xor##
  (-)         = xor##
  (*)         = and##
  negate      = complement##
  abs         = id
  signum b    = b
  fromInteger = fromInteger##

fromInteger## :: Integer -> Bit
fromInteger## i = Bit (i `mod` 2)
{-# NOINLINE fromInteger## #-}

instance Real Bit where
  toRational b = if eq## b low then 0 else 1

instance Integral Bit where
  quot    a _ = a
  rem     _ _ = low
  div     a _ = a
  mod     _ _ = low
  quotRem n _ = (n,low)
  divMod  n _ = (n,low)
  toInteger b = if eq## b low then 0 else 1

instance Bits Bit where
  (.&.)             = and##
  (.|.)             = or##
  xor               = xor##
  complement        = complement##
  zeroBits          = low
  bit i             = if i == 0 then high else low
  setBit _ i        = if i == 0 then high else low
  clearBit _ i      = if i == 0 then low  else high
  complementBit b i = if i == 0 then complement## b else b
  testBit b i       = if i == 0 then eq## b high else False
  bitSizeMaybe _    = Just 1
  bitSize _         = 1
  isSigned _        = False
  shiftL b i        = if i == 0 then b else low
  shiftR b i        = if i == 0 then b else low
  rotateL b _       = b
  rotateR b _       = b
  popCount b        = if eq## b low then 0 else 1

instance FiniteBits Bit where
  finiteBitSize _      = 1
  countLeadingZeros b  = if eq## b low then 1 else 0
  countTrailingZeros b = if eq## b low then 1 else 0

and##, or##, xor## :: Bit -> Bit -> Bit
and## (Bit v1) (Bit v2) = Bit (v1 .&. v2)
{-# NOINLINE and## #-}

or## (Bit v1) (Bit v2) = Bit (v1 .|. v2)
{-# NOINLINE or## #-}

xor## (Bit v1) (Bit v2) = Bit (v1 `xor` v2)
{-# NOINLINE xor## #-}

complement## :: Bit -> Bit
complement## (Bit 0) = Bit 1
complement## _       = Bit 0
{-# NOINLINE complement## #-}

-- *** BitPack
pack# :: Bit -> BitVector 1
pack# (Bit b) = BV b
{-# NOINLINE pack# #-}

unpack# :: BitVector 1 -> Bit
unpack# (BV b) = Bit b
{-# NOINLINE unpack# #-}

-- * Instances
instance NFData (BitVector n) where
  rnf (BV i) = rnf i `seq` ()
  {-# NOINLINE rnf #-}
  -- NOINLINE is needed so that Clash doesn't trip on the "BitVector ~# Integer"
  -- coercion

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
  {-# NOINLINE show #-}

instance KnownNat n => ShowX (BitVector n) where
  showsPrecX = showsPrecXWith showsPrec

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
enumFromTo#     :: BitVector n -> BitVector n -> [BitVector n]
enumFromThenTo# :: BitVector n -> BitVector n -> BitVector n -> [BitVector n]
enumFrom# x             = map fromInteger_INLINE [unsafeToInteger x ..]
enumFromThen# x y       = map fromInteger_INLINE [unsafeToInteger x, unsafeToInteger y ..]
enumFromTo# x y         = map BV [unsafeToInteger x .. unsafeToInteger y]
enumFromThenTo# x1 x2 y = map BV [unsafeToInteger x1, unsafeToInteger x2 .. unsafeToInteger y]

instance KnownNat n => Bounded (BitVector n) where
  minBound = minBound#
  maxBound = maxBound#

{-# NOINLINE minBound# #-}
minBound# :: BitVector n
minBound# = BV 0

{-# NOINLINE maxBound# #-}
maxBound# :: forall n . KnownNat n => BitVector n
maxBound# = let m = 1 `shiftL` fromInteger (natVal (Proxy @n))
            in  BV (m-1)

instance KnownNat n => Num (BitVector n) where
  (+)         = (+#)
  (-)         = (-#)
  (*)         = (*#)
  negate      = negate#
  abs         = id
  signum bv   = resize# (pack# (reduceOr# bv))
  fromInteger = fromInteger#

(+#),(-#),(*#) :: forall n . KnownNat n => BitVector n -> BitVector n -> BitVector n
{-# NOINLINE (+#) #-}
(+#) (BV i) (BV j) = let m = 1 `shiftL` fromInteger (natVal (Proxy @n))
                         z = i + j
                     in  if z >= m then BV (z - m) else BV z

{-# NOINLINE (-#) #-}
(-#) (BV i) (BV j) = let m = 1 `shiftL` fromInteger (natVal (Proxy @n))
                         z = i - j
                     in  if z < 0 then BV (m + z) else BV z

{-# NOINLINE (*#) #-}
(*#) (BV i) (BV j) = fromInteger_INLINE (i * j)

{-# NOINLINE negate# #-}
negate# :: forall n . KnownNat n => BitVector n -> BitVector n
negate# (BV 0) = BV 0
negate# (BV i) = BV (sz - i)
  where
    sz = 1 `shiftL` fromInteger (natVal (Proxy @n))

{-# NOINLINE fromInteger# #-}
fromInteger# :: KnownNat n => Integer -> BitVector n
fromInteger# = fromInteger_INLINE

{-# INLINE fromInteger_INLINE #-}
fromInteger_INLINE :: forall n . KnownNat n => Integer -> BitVector n
fromInteger_INLINE i = sz `seq` BV (i `mod` sz)
  where
    sz = 1 `shiftL` fromInteger (natVal (Proxy @n))

instance (KnownNat m, KnownNat n) => ExtendingNum (BitVector m) (BitVector n) where
  type AResult (BitVector m) (BitVector n) = BitVector (Max m n + 1)
  plus  = plus#
  minus = minus#
  type MResult (BitVector m) (BitVector n) = BitVector (m + n)
  times = times#

{-# NOINLINE plus# #-}
plus# :: BitVector m -> BitVector n -> BitVector (Max m n + 1)
plus# (BV a) (BV b) = BV (a + b)

{-# NOINLINE minus# #-}
minus# :: forall m n . (KnownNat m, KnownNat n) => BitVector m -> BitVector n
                                                -> BitVector (Max m n + 1)
minus# (BV a) (BV b) =
  let sz   = fromInteger (natVal (Proxy @(Max m n + 1)))
      mask = 1 `shiftL` sz
      z    = a - b
  in  if z < 0 then BV (mask + z) else BV z

{-# NOINLINE times# #-}
times# :: BitVector m -> BitVector n -> BitVector (m + n)
times# (BV a) (BV b) = BV (a * b)

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
  complementBit v i = replaceBit# v i (complement## (index# v i))
  testBit v i       = eq## (index# v i) high
  bitSizeMaybe v    = Just (size# v)
  bitSize           = size#
  isSigned _        = False
  shiftL v i        = shiftL# v i
  shiftR v i        = shiftR# v i
  rotateL v i       = rotateL# v i
  rotateR v i       = rotateR# v i
  popCount bv       = fromInteger (I.toInteger# (popCountBV (bv ++# (0 :: BitVector 1))))

instance KnownNat n => FiniteBits (BitVector n) where
  finiteBitSize       = size#
  countLeadingZeros   = fromInteger . I.toInteger# . countLeadingZerosBV
  countTrailingZeros  = fromInteger . I.toInteger# . countTrailingZerosBV

countLeadingZerosBV :: KnownNat n => BitVector n -> I.Index (n+1)
countLeadingZerosBV = V.foldr (\l r -> if eq## l low then 1 + r else 0) 0 . V.bv2v
{-# INLINE countLeadingZerosBV #-}

countTrailingZerosBV :: KnownNat n => BitVector n -> I.Index (n+1)
countTrailingZerosBV = V.foldl (\l r -> if eq## r low then 1 + l else 0) 0 . V.bv2v
{-# INLINE countTrailingZerosBV #-}

{-# NOINLINE reduceAnd# #-}
reduceAnd# :: KnownNat n => BitVector n -> Bit
reduceAnd# bv@(BV i) = Bit (smallInteger (dataToTag# check))
  where
    check = i == maxI

    sz    = natVal bv
    maxI  = (2 ^ sz) - 1

{-# NOINLINE reduceOr# #-}
reduceOr# :: BitVector n -> Bit
reduceOr# (BV i) = Bit (smallInteger (dataToTag# check))
  where
    check = i /= 0

{-# NOINLINE reduceXor# #-}
reduceXor# :: BitVector n -> Bit
reduceXor# (BV i) = Bit (toInteger (popCount i `mod` 2))

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
    | i >= 0 && i < sz = Bit (smallInteger
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
msb# :: forall n . KnownNat n => BitVector n -> Bit
msb# (BV v)
  = let i = fromInteger (natVal (Proxy @n) - 1)
    in  Bit (smallInteger (dataToTag# (testBit v i)))

{-# NOINLINE lsb# #-}
-- | LSB
lsb# :: BitVector n -> Bit
lsb# (BV v) = Bit (smallInteger (dataToTag# (testBit v 0)))

{-# NOINLINE slice# #-}
slice# :: BitVector (m + 1 + i) -> SNat m -> SNat n -> BitVector (m + 1 - n)
slice# (BV i) m n = BV (shiftR (i .&. mask) n')
  where
    m' = snatToInteger m
    n' = snatToNum n

    mask = 2 ^ (m' + 1) - 1

-- * Constructions

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
replaceBit# bv@(BV v) i (Bit b)
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
split# :: forall n m . KnownNat n
       => BitVector (m + n) -> (BitVector m, BitVector n)
split# (BV i) = (BV l, BV r)
  where
    n     = fromInteger (natVal (Proxy @n))
    mask  = 1 `shiftL` n
    -- The code below is faster than:
    -- > (l,r) = i `divMod` mask
    r    = i `mod` mask
    l    = i `shiftR` n

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

shiftL#, shiftR#, rotateL#, rotateR#
  :: KnownNat n => BitVector n -> Int -> BitVector n

{-# NOINLINE shiftL# #-}
shiftL# (BV v) i
  | i < 0     = error
              $ "'shiftL undefined for negative number: " ++ show i
  | otherwise = fromInteger_INLINE (shiftL v i)

{-# NOINLINE shiftR# #-}
shiftR# (BV v) i
  | i < 0     = error
              $ "'shiftR undefined for negative number: " ++ show i
  | otherwise = BV (shiftR v i)

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

popCountBV :: forall n . KnownNat n => BitVector (n+1) -> I.Index (n+2)
popCountBV bv =
  let v = V.bv2v bv
  in  sum (V.map (fromIntegral . pack#) v)
{-# INLINE popCountBV #-}

instance Resize BitVector where
  resize     = resize#
  zeroExtend = extend
  signExtend = \bv -> (if msb# bv == low then id else complement) 0 ++# bv
  truncateB  = resize#

{-# NOINLINE resize# #-}
resize# :: forall n m . KnownNat m => BitVector n -> BitVector m
resize# (BV i) = let m = 1 `shiftL` fromInteger (natVal (Proxy @m))
                 in  if i >= m then fromInteger_INLINE i else BV i

instance KnownNat n => Lift (BitVector n) where
  lift bv@(BV i) = sigE [| fromInteger# i |] (decBitVector (natVal bv))
  {-# NOINLINE lift #-}

decBitVector :: Integer -> TypeQ
decBitVector n = appT (conT ''BitVector) (litT $ numTyLit n)

instance KnownNat n => SaturatingNum (BitVector n) where
  satPlus SatWrap a b = a +# b
  satPlus SatZero a b =
    let r = plus# a b
    in  if msb# r == low
           then resize# r
           else minBound#
  satPlus _ a b =
    let r  = plus# a b
    in  if msb# r == low
           then resize# r
           else maxBound#

  satMin SatWrap a b = a -# b
  satMin _ a b =
    let r = minus# a b
    in  if msb# r == low
           then resize# r
           else minBound#

  satMult SatWrap a b = a *# b
  satMult SatZero a b =
    let r       = times# a b
        (rL,rR) = split# r
    in  case rL of
          0 -> rR
          _ -> minBound#
  satMult _ a b =
    let r       = times# a b
        (rL,rR) = split# r
    in  case rL of
          0 -> rR
          _ -> maxBound#

instance KnownNat n => Arbitrary (BitVector n) where
  arbitrary = arbitraryBoundedIntegral
  shrink    = shrinkSizedUnsigned

-- | 'shrink' for sized unsigned types
shrinkSizedUnsigned :: (KnownNat n, Integral (p n)) => p n -> [p n]
shrinkSizedUnsigned x | natVal x < 2 = case toInteger x of
                                         1 -> [0]
                                         _ -> []
                      -- 'shrinkIntegral' uses "`quot` 2", which for sized types
                      -- less than 2 bits wide results in a division by zero.
                      --
                      -- See: https://github.com/clash-lang/clash-compiler/issues/153
                      | otherwise    = shrinkIntegral x
{-# INLINE shrinkSizedUnsigned #-}

instance KnownNat n => CoArbitrary (BitVector n) where
  coarbitrary = coarbitraryIntegral

type instance Index   (BitVector n) = Int
type instance IxValue (BitVector n) = Bit
instance KnownNat n => Ixed (BitVector n) where
  ix i f bv = replaceBit# bv i <$> f (index# bv i)
