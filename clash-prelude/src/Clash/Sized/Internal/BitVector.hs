{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2019     , Gergő Érdi
                  2016-2019, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
  , undefined#
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
  , isLike
    -- *** Ord
  , lt#
  , ge#
  , gt#
  , le#
    -- *** Enum (not synthesizable)
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
  , truncateB#
    -- *** QuickCheck
  , shrinkSizedUnsigned
  -- ** Other
  , undefError
  , checkUnpackUndef
  , bitPattern
  )
where

import Control.DeepSeq            (NFData (..))
import Control.Lens               (Index, Ixed (..), IxValue)
import Data.Bits                  (Bits (..), FiniteBits (..))
import Data.Data                  (Data)
import Data.Default.Class         (Default (..))
import Data.Either                (isLeft)
import Data.Proxy                 (Proxy (..))
import Data.Typeable              (Typeable, typeOf)
import GHC.Generics               (Generic)
import Data.Maybe                 (fromMaybe)
import GHC.Exts
  (Word#, Word (W#), eqWord#, int2Word#, uncheckedShiftRL#)
import qualified GHC.Exts
import GHC.Integer.GMP.Internals  (Integer (..), bigNatToWord, shiftRBigNat)
import GHC.Natural
  (Natural (..), naturalFromInteger, wordToNatural)
#if MIN_VERSION_base(4,12,0)
import GHC.Natural                (naturalToInteger)
#endif
import GHC.Prim                   (dataToTag#)
import GHC.Stack                  (HasCallStack, withFrozenCallStack)
import GHC.TypeLits               (KnownNat, Nat, type (+), type (-), natVal)
import GHC.TypeLits.Extra         (Max)
import Language.Haskell.TH        (Q, TExp, TypeQ, appT, conT, litT, numTyLit, sigE, Lit(..), litE, Pat, litP)
import Language.Haskell.TH.Syntax (Lift(..))
import Test.QuickCheck.Arbitrary  (Arbitrary (..), CoArbitrary (..),
                                   arbitraryBoundedIntegral,
                                   coarbitraryIntegral, shrinkIntegral)

import Clash.Class.Num            (ExtendingNum (..), SaturatingNum (..),
                                   SaturationMode (..))
import Clash.Class.Resize         (Resize (..))
import Clash.Promoted.Nat
  (SNat (..), SNatLE (..), compareSNat, snatToInteger, snatToNum, natToNum)
import Clash.XException
  (ShowX (..), NFDataX (..), errorX, isX, showsPrecXWith, rwhnfX)

import Clash.Sized.Internal.Mod

import {-# SOURCE #-} qualified Clash.Sized.Vector         as V
import {-# SOURCE #-} qualified Clash.Sized.Internal.Index as I
import                qualified Data.List                  as L

{- $setup
>>> :set -XTemplateHaskell
>>> :set -XBinaryLiterals
>>> import Clash.Sized.Internal.BitVector
-}

-- * Type definitions

-- | A vector of bits.
--
-- * Bit indices are descending
-- * 'Num' instance performs /unsigned/ arithmetic.
data BitVector (n :: Nat) =
    -- | The constructor, 'BV', and  the field, 'unsafeToInteger', are not
    -- synthesizable.
    BV { unsafeMask      :: !Natural
       , unsafeToNatural :: !Natural
       }
  deriving (Data, Generic)

-- * Bit

-- | Bit
data Bit =
  -- | The constructor, 'Bit', and  the field, 'unsafeToInteger#', are not
  -- synthesizable.
  Bit { unsafeMask#      :: {-# unpack #-} !Word
      , unsafeToInteger# :: {-# unpack #-} !Word
      }
  deriving (Data, Generic)

-- * Constructions
-- ** Initialisation
{-# NOINLINE high #-}
-- | logic '1'
high :: Bit
high = Bit 0 1

{-# NOINLINE low #-}
-- | logic '0'
low :: Bit
low = Bit 0 0

-- ** Instances
instance NFData Bit where
  rnf (Bit m i) = rnf m `seq` rnf i `seq` ()
  {-# NOINLINE rnf #-}

instance Show Bit where
  show (Bit 0 b) =
    case testBit b 0 of
      True  -> "1"
      False -> "0"
  show (Bit _ _) = "."

instance ShowX Bit where
  showsPrecX = showsPrecXWith showsPrec

instance NFDataX Bit where
  deepErrorX = errorX
  rnfX = rwhnfX
  hasUndefined bv = isLeft (isX bv) || unsafeMask# bv /= 0

instance Lift Bit where
  lift (Bit m i) = [| fromInteger## $(litE (WordPrimL (toInteger m))) i |]
  {-# NOINLINE lift #-}

instance Eq Bit where
  (==) = eq##
  (/=) = neq##

eq## :: Bit -> Bit -> Bool
eq## b1 b2 = eq# (pack# b1) (pack# b2)
{-# NOINLINE eq## #-}

neq## :: Bit -> Bit -> Bool
neq## b1 b2 = neq# (pack# b1) (pack# b2)
{-# NOINLINE neq## #-}

instance Ord Bit where
  (<)  = lt##
  (<=) = le##
  (>)  = gt##
  (>=) = ge##

lt##,ge##,gt##,le## :: Bit -> Bit -> Bool
lt## b1 b2 = lt# (pack# b1) (pack# b2)
{-# NOINLINE lt## #-}
ge## b1 b2 = ge# (pack# b1) (pack# b2)
{-# NOINLINE ge## #-}
gt## b1 b2 = gt# (pack# b1) (pack# b2)
{-# NOINLINE gt## #-}
le## b1 b2 = le# (pack# b1) (pack# b2)
{-# NOINLINE le## #-}

instance Enum Bit where
  toEnum     = fromInteger## 0## . toInteger
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
  fromInteger = fromInteger## 0##

fromInteger## :: Word# -> Integer -> Bit
fromInteger## m# i = Bit ((W# m#) `mod` 2) (fromInteger i `mod` 2)
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
  setBit b i        = if i == 0 then high else b
  clearBit b i      = if i == 0 then low  else b
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
and## (Bit m1 v1) (Bit m2 v2) = Bit mask (v1 .&. v2 .&. complement mask)
  where mask = (m1.&.v2 .|. m1.&.m2 .|. m2.&.v1)
{-# NOINLINE and## #-}

or## (Bit m1 v1) (Bit m2 v2) = Bit mask ((v1 .|. v2) .&. complement mask)
  where mask = m1 .&. complement v2 .|.  m1.&.m2  .|.  m2 .&. complement v1
{-# NOINLINE or## #-}

xor## (Bit m1 v1) (Bit m2 v2) = Bit mask ((v1 `xor` v2) .&. complement mask)
  where mask = m1 .|. m2
{-# NOINLINE xor## #-}

complement## :: Bit -> Bit
complement## (Bit m v) = Bit m (complementB v .&. complementB m)
  where complementB (W# b#) = W# (int2Word# (eqWord# b# 0##))
{-# NOINLINE complement## #-}

-- *** BitPack
pack# :: Bit -> BitVector 1
pack# (Bit (W# m) (W# b)) = BV (NatS# m) (NatS# b)
{-# NOINLINE pack# #-}

unpack# :: BitVector 1 -> Bit
unpack# (BV m b) = Bit (go m) (go b)
 where
  go (NatS# w) = W# w
  go (NatJ# w) = W# (bigNatToWord w)
{-# NOINLINE unpack# #-}

-- * Instances
instance NFData (BitVector n) where
  rnf (BV i m) = rnf i `seq` rnf m `seq` ()
  {-# NOINLINE rnf #-}
  -- NOINLINE is needed so that Clash doesn't trip on the "BitVector ~# Integer"
  -- coercion

instance KnownNat n => Show (BitVector n) where
  show bv@(BV msk i) = reverse . underScore . reverse $ showBV (natVal bv) msk i []
    where
      showBV 0 _ _ s = s
      showBV n m v s = let (v',vBit) = divMod v 2
                           (m',mBit) = divMod m 2
                       in  case (mBit,vBit) of
                           (0,0) -> showBV (n - 1) m' v' ('0':s)
                           (0,_) -> showBV (n - 1) m' v' ('1':s)
                           _     -> showBV (n - 1) m' v' ('.':s)

      underScore xs = case splitAt 5 xs of
                        ([a,b,c,d,e],rest) -> [a,b,c,d,'_'] ++ underScore (e:rest)
                        (rest,_)               -> rest
  {-# NOINLINE show #-}

instance KnownNat n => ShowX (BitVector n) where
  showsPrecX = showsPrecXWith showsPrec

instance NFDataX (BitVector n) where
  deepErrorX = errorX
  rnfX = rwhnfX
  hasUndefined bv = isLeft (isX bv) || unsafeMask bv /= 0

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
--
-- Also 'bLit' can handle don't care bits:
--
-- >>> $$(bLit "1.0.") :: BitVector 4
-- 1.0.
bLit :: forall n. KnownNat n => String -> Q (TExp (BitVector n))
bLit s = [|| fromInteger# m i1 ||]
  where
    bv :: BitVector n
    bv = read# s

    m,i :: Natural
    BV m i = bv

    i1 :: Integer
    i1 = toInteger i

read# :: KnownNat n => String -> BitVector n
read# cs = BV m v
  where
    (vs,ms) = unzip . map readBit . filter (/= '_') $ cs
    combineBits = foldl (\b a -> b*2+a) 0
    v = combineBits vs
    m = combineBits ms
    readBit c = case c of
      '0' -> (0,0)
      '1' -> (1,0)
      '.' -> (0,1)
      _   -> error $ "Clash.Sized.Internal.bLit: unknown character: " ++ show c ++ " in input: " ++ cs


instance KnownNat n => Eq (BitVector n) where
  (==) = eq#
  (/=) = neq#

{-# NOINLINE eq# #-}
eq# :: KnownNat n => BitVector n -> BitVector n -> Bool
eq# (BV 0 v1) (BV 0 v2 ) = v1 == v2
eq# bv1 bv2 = undefErrorI "==" bv1 bv2

{-# NOINLINE neq# #-}
neq# :: KnownNat n => BitVector n -> BitVector n -> Bool
neq# (BV 0 v1) (BV 0 v2) = v1 /= v2
neq# bv1 bv2 = undefErrorI "/=" bv1 bv2

instance KnownNat n => Ord (BitVector n) where
  (<)  = lt#
  (>=) = ge#
  (>)  = gt#
  (<=) = le#

lt#,ge#,gt#,le# :: KnownNat n => BitVector n -> BitVector n -> Bool
{-# NOINLINE lt# #-}
lt# (BV 0 n) (BV 0 m) = n < m
lt# bv1 bv2 = undefErrorI "<" bv1 bv2
{-# NOINLINE ge# #-}
ge# (BV 0 n) (BV 0 m) = n >= m
ge# bv1 bv2 = undefErrorI ">=" bv1 bv2
{-# NOINLINE gt# #-}
gt# (BV 0 n) (BV 0 m) = n > m
gt# bv1 bv2 = undefErrorI ">" bv1 bv2
{-# NOINLINE le# #-}
le# (BV 0 n) (BV 0 m) = n <= m
le#  bv1 bv2 = undefErrorI "<=" bv1 bv2

-- | The functions: 'enumFrom', 'enumFromThen', 'enumFromTo', and
-- 'enumFromThenTo', are not synthesizable.
instance KnownNat n => Enum (BitVector n) where
  succ           = (+# fromInteger# 0 1)
  pred           = (-# fromInteger# 0 1)
  toEnum         = fromInteger# 0 . toInteger
  fromEnum       = fromEnum . toInteger#
  enumFrom       = enumFrom#
  enumFromThen   = enumFromThen#
  enumFromTo     = enumFromTo#
  enumFromThenTo = enumFromThenTo#

enumFrom# :: forall n. KnownNat n => BitVector n -> [BitVector n]
enumFrom# (BV 0 x) = map (BV 0 . (`mod` m)) [x .. unsafeToNatural (maxBound :: BitVector n)]
  where m = 1 `shiftL` fromInteger (natVal (Proxy @n))
enumFrom# bv = undefErrorU "enumFrom" bv
{-# NOINLINE enumFrom# #-}

enumFromThen#
  :: forall n
   . KnownNat n
  => BitVector n
  -> BitVector n
  -> [BitVector n]
enumFromThen# (BV 0 x) (BV 0 y) =
  toBvs [x, y .. unsafeToNatural bound]
 where
  bound = if x <= y then maxBound else minBound :: BitVector n
  toBvs = map (BV 0 . (`mod` m))
  m = 1 `shiftL` fromInteger (natVal (Proxy @n))
enumFromThen# bv1 bv2 = undefErrorP "enumFromThen" bv1 bv2
{-# NOINLINE enumFromThen# #-}

enumFromTo#
  :: forall n
   . KnownNat n
  => BitVector n
  -> BitVector n
  -> [BitVector n]
enumFromTo# (BV 0 x) (BV 0 y) = map (BV 0 . (`mod` m)) [x .. y]
  where m = 1 `shiftL` fromInteger (natVal (Proxy @n))
enumFromTo# bv1 bv2 = undefErrorP "enumFromTo" bv1 bv2
{-# NOINLINE enumFromTo# #-}

enumFromThenTo#
  :: forall n
   . KnownNat n
  => BitVector n
  -> BitVector n
  -> BitVector n
  -> [BitVector n]
enumFromThenTo# (BV 0 x1) (BV 0 x2) (BV 0 y) = map (BV 0 . (`mod` m)) [x1, x2 .. y]
  where m = 1 `shiftL` fromInteger (natVal (Proxy @n))
enumFromThenTo# bv1 bv2 bv3 = undefErrorP3 "enumFromTo" bv1 bv2 bv3
{-# NOINLINE enumFromThenTo# #-}


instance KnownNat n => Bounded (BitVector n) where
  minBound = minBound#
  maxBound = maxBound#

minBound# :: BitVector n
minBound# = BV 0 0
{-# NOINLINE minBound# #-}

maxBound# :: forall n. KnownNat n => BitVector n
maxBound# = let m = 1 `shiftL` natToNum @n in BV 0 (m-1)
{-# NOINLINE maxBound# #-}

instance KnownNat n => Num (BitVector n) where
  (+)         = (+#)
  (-)         = (-#)
  (*)         = (*#)
  negate      = negate#
  abs         = id
  signum bv   = resizeBV (pack# (reduceOr# bv))
  fromInteger = fromInteger# 0

(+#),(-#),(*#) :: forall n . KnownNat n => BitVector n -> BitVector n -> BitVector n
{-# NOINLINE (+#) #-}
(+#) = go
  where
    go (BV 0 i) (BV 0 j) = BV 0 (addMod m i j)
    go bv1 bv2 = undefErrorI "+" bv1 bv2

    m = 1 `shiftL` fromInteger (natVal (Proxy @n))

{-# NOINLINE (-#) #-}
(-#) = go
  where
    go (BV 0 i) (BV 0 j) = BV 0 (subMod m i j)
    go bv1 bv2 = undefErrorI "-" bv1 bv2

    m = 1 `shiftL` fromInteger (natVal (Proxy @n))

{-# NOINLINE (*#) #-}
(*#) = go
 where
  go (BV 0 i) (BV 0 j) = BV 0 (mulMod2 m i j)
  go bv1 bv2 = undefErrorI "*" bv1 bv2

  m = (1 `shiftL` fromInteger (natVal (Proxy @n))) - 1

{-# NOINLINE negate# #-}
negate# :: forall n . KnownNat n => BitVector n -> BitVector n
negate# = go
 where
  go (BV 0 i) = BV 0 (negateMod m i)
  go bv = undefErrorU "negate" bv

  m = 1 `shiftL` fromInteger (natVal (Proxy @n))

{-# NOINLINE fromInteger# #-}
fromInteger# :: KnownNat n => Natural -> Integer -> BitVector n
fromInteger# m i = sz `seq` mx
  where
    mx = BV (m `mod` naturalFromInteger sz)
            (naturalFromInteger (i `mod` sz))
    sz  = 1 `shiftL` fromInteger (natVal mx) :: Integer

instance (KnownNat m, KnownNat n) => ExtendingNum (BitVector m) (BitVector n) where
  type AResult (BitVector m) (BitVector n) = BitVector (Max m n + 1)
  add  = plus#
  sub = minus#
  type MResult (BitVector m) (BitVector n) = BitVector (m + n)
  mul = times#

{-# NOINLINE plus# #-}
plus# :: (KnownNat m, KnownNat n) => BitVector m -> BitVector n -> BitVector (Max m n + 1)
plus# (BV 0 a) (BV 0 b) = BV 0 (a + b)
plus# bv1 bv2 = undefErrorP "add" bv1 bv2

{-# NOINLINE minus# #-}
minus# :: forall m n . (KnownNat m, KnownNat n) => BitVector m -> BitVector n
                                                -> BitVector (Max m n + 1)
minus# = go
 where
  go (BV 0 a) (BV 0 b) = BV 0 (subMod m a b)
  go bv1 bv2 = undefErrorP "sub" bv1 bv2

  m = 1 `shiftL` fromInteger (natVal (Proxy @(Max m n + 1)))

{-# NOINLINE times# #-}
times# :: (KnownNat m, KnownNat n) => BitVector m -> BitVector n -> BitVector (m + n)
times# (BV 0 a) (BV 0 b) = BV 0 (a * b)
times# bv1 bv2 = undefErrorP "mul" bv1 bv2

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

quot#,rem# :: KnownNat n => BitVector n -> BitVector n -> BitVector n
{-# NOINLINE quot# #-}
quot# (BV 0 i) (BV 0 j) = BV 0 (i `quot` j)
quot# bv1 bv2 = undefErrorP "quot" bv1 bv2
{-# NOINLINE rem# #-}
rem# (BV 0 i) (BV 0 j) = BV 0 (i `rem` j)
rem# bv1 bv2 = undefErrorP "rem" bv1 bv2

{-# NOINLINE toInteger# #-}
toInteger# :: KnownNat n => BitVector n -> Integer
toInteger# (BV 0 i) = naturalToInteger i
toInteger# bv = undefErrorU "toInteger" bv

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
reduceAnd# bv@(BV 0 i) = Bit 0 (W# (int2Word# (dataToTag# check)))
  where
    check = i == maxI

    sz    = natVal bv
    maxI  = (2 ^ sz) - 1
reduceAnd# bv = V.foldl (.&.) 1 (V.bv2v bv)

{-# NOINLINE reduceOr# #-}
reduceOr# :: KnownNat n => BitVector n -> Bit
reduceOr# (BV 0 i) = Bit 0 (W# (int2Word# (dataToTag# check)))
  where
    check = i /= 0
reduceOr# bv = V.foldl (.|.) 0 (V.bv2v bv)

{-# NOINLINE reduceXor# #-}
reduceXor# :: KnownNat n => BitVector n -> Bit
reduceXor# (BV 0 i) = Bit 0 (fromIntegral (popCount i `mod` 2))
reduceXor# bv = undefErrorU "reduceXor" bv

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
index# bv@(BV m v) i
    | i >= 0 && i < sz = Bit (W# (int2Word# (dataToTag# (testBit m i))))
                             (W# (int2Word# (dataToTag# (testBit v i))))
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
msb# (BV m v)
  = Bit (msbN m)
        (msbN v)
 where
  !(S# i#) = natVal (Proxy @n)
  msbN (NatS# w)  = W# (w `uncheckedShiftRL#` (i# GHC.Exts.-# 1#))
  msbN (NatJ# bn) = W# (bigNatToWord (shiftRBigNat bn (i# GHC.Exts.-# 1#)))

{-# NOINLINE lsb# #-}
-- | LSB
lsb# :: BitVector n -> Bit
lsb# (BV m v) = Bit (W# (int2Word# (dataToTag# (testBit m 0))))
                    (W# (int2Word# (dataToTag# (testBit v 0))))

{-# NOINLINE slice# #-}
slice# :: BitVector (m + 1 + i) -> SNat m -> SNat n -> BitVector (m + 1 - n)
slice# (BV msk i) m n = BV (shiftR (msk .&. mask) n')
                           (shiftR (i   .&. mask) n')
  where
    m' = snatToInteger m
    n' = snatToNum n

    mask = 2 ^ (m' + 1) - 1

-- * Constructions

-- ** Concatenation
{-# NOINLINE (++#) #-}
-- | Concatenate two 'BitVector's
(++#) :: KnownNat m => BitVector n -> BitVector m -> BitVector (n + m)
(BV m1 v1) ++# bv2@(BV m2 v2) = BV (m1' .|. m2) (v1' .|. v2)
  where
    size2 = fromInteger (natVal bv2)
    v1' = shiftL v1 size2
    m1' = shiftL m1 size2

-- * Modifying BitVectors
{-# NOINLINE replaceBit# #-}
replaceBit# :: KnownNat n => BitVector n -> Int -> Bit -> BitVector n
replaceBit# bv@(BV m v) i (Bit mb b)
    | i >= 0 && i < sz = BV (clearBit m i  .|. (wordToNatural mb `shiftL` i))
                            (if testBit b 0 && mb == 0 then setBit v i else clearBit v i)
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
setSlice#
  :: forall m i n
   . SNat (m + 1 + i)
  -> BitVector (m + 1 + i)
  -> SNat m
  -> SNat n
  -> BitVector (m + 1 - n)
  -> BitVector (m + 1 + i)
setSlice# SNat =
  \(BV iMask i) m@SNat n (BV jMask j) ->
    let m' = snatToInteger m
        n' = snatToInteger n

        j'     = shiftL j     (fromInteger n')
        jMask' = shiftL jMask (fromInteger n')
        mask   = complementN ((2 ^ (m' + 1) - 1) `xor` (2 ^ n' - 1))
    in  BV ((iMask .&. mask) .|. jMask') ((i .&. mask) .|. j')
 where
  complementN = complementMod (natVal (Proxy @(m + 1 + i)))

{-# NOINLINE split# #-}
split#
  :: forall n m
   . KnownNat n
  => BitVector (m + n)
  -> (BitVector m, BitVector n)
split# (BV m i) =
  let n     = fromInteger (natVal (Proxy @n))
      mask  = maskMod (natVal (Proxy @n))
      r     = mask i
      rMask = mask m
      l     = i `shiftR` n
      lMask = m `shiftR` n
  in  (BV lMask l, BV rMask r)

and#, or#, xor# :: forall n . KnownNat n => BitVector n -> BitVector n -> BitVector n
{-# NOINLINE and# #-}
and# =
  \(BV m1 v1) (BV m2 v2) ->
    let mask = (m1.&.v2 .|. m1.&.m2 .|. m2.&.v1)
    in  BV mask (v1 .&. v2  .&. complementN mask)
  where
    complementN = complementMod (natVal (Proxy @n))

{-# NOINLINE or# #-}
or# =
  \(BV m1 v1) (BV m2 v2) ->
    let mask = m1 .&. complementN v2  .|.  m1.&.m2  .|.  m2 .&. complementN v1
    in  BV mask ((v1.|.v2) .&. complementN mask)
  where
    complementN = complementMod (natVal (Proxy @n))

{-# NOINLINE xor# #-}
xor# =
  \(BV m1 v1) (BV m2 v2) ->
    let mask  = m1 .|. m2
    in  BV mask ((v1 `xor` v2) .&. complementN mask)
  where
    complementN = complementMod (natVal (Proxy @n))

{-# NOINLINE complement# #-}
complement# :: forall n . KnownNat n => BitVector n -> BitVector n
complement# = \(BV m v) -> BV m (complementN v .&. complementN m)
  where complementN = complementMod (natVal (Proxy @n))

shiftL#, shiftR#, rotateL#, rotateR#
  :: forall n . KnownNat n => BitVector n -> Int -> BitVector n

{-# NOINLINE shiftL# #-}
shiftL# =
  \(BV msk v) i ->
    if i >= 0 then
      BV ((shiftL msk i) `mod` m) ((shiftL v i) `mod` m)
    else
      error ("'shiftL' undefined for negative number: " ++ show i)
 where
  m = 1 `shiftL` fromInteger (natVal (Proxy @n))

{-# NOINLINE shiftR# #-}
shiftR# (BV m v) i
  | i < 0     = error
              $ "'shiftR undefined for negative number: " ++ show i
  | otherwise = BV (shiftR m i) (shiftR v i)

{-# NOINLINE rotateL# #-}
rotateL# =
  \(BV msk v) b ->
    if b >= 0 then
      let vl    = shiftL v b'
          vr    = shiftR v b''

          ml    = shiftL msk b'
          mr    = shiftR msk b''

          b'   = b `mod` sz
          b''  = sz - b'
      in  BV ((ml .|. mr) `mod` m) ((vl .|. vr) `mod` m)
    else
      error "'rotateL' undefined for negative numbers"
 where
  sz = fromInteger (natVal (Proxy @n)) :: Int
  m  = 1 `shiftL` sz

{-# NOINLINE rotateR# #-}
rotateR# =
  \(BV msk v) b ->
    if b >= 0 then
      let vl   = shiftR v b'
          vr   = shiftL v b''
          ml   = shiftR msk b'
          mr   = shiftL msk b''
          b'   = b `mod` sz
          b''  = sz - b'
      in  BV ((ml .|. mr) `mod` m) ((vl .|. vr) `mod` m)
    else
      error "'rotateR' undefined for negative numbers"
 where
  sz = fromInteger (natVal (Proxy @n)) :: Int
  m  = 1 `shiftL` sz

popCountBV :: forall n . KnownNat n => BitVector (n+1) -> I.Index (n+2)
popCountBV bv =
  let v = V.bv2v bv
  in  sum (V.map (fromIntegral . pack#) v)
{-# INLINE popCountBV #-}

instance Resize BitVector where
  resize     = resizeBV
  zeroExtend = (0 ++#)
  signExtend = \bv -> (if msb# bv == low then id else complement) 0 ++# bv
  truncateB  = truncateB#

resizeBV :: forall n m . (KnownNat n, KnownNat m) => BitVector n -> BitVector m
resizeBV = case compareSNat @n @m (SNat @n) (SNat @m) of
  SNatLE -> (++#) @n @(m-n) 0
  SNatGT -> truncateB# @m @(n - m)
{-# INLINE resizeBV #-}

truncateB# :: forall a b . KnownNat a => BitVector (a + b) -> BitVector a
truncateB# = \(BV msk i) -> BV (msk `mod` m) (i `mod` m)
  where m = 1 `shiftL` fromInteger (natVal (Proxy @a))
{-# NOINLINE truncateB# #-}

instance KnownNat n => Lift (BitVector n) where
  lift bv@(BV m i) = sigE [| fromInteger# m $(litE (IntegerL (toInteger i))) |] (decBitVector (natVal bv))
  {-# NOINLINE lift #-}

decBitVector :: Integer -> TypeQ
decBitVector n = appT (conT ''BitVector) (litT $ numTyLit n)

instance KnownNat n => SaturatingNum (BitVector n) where
  satAdd SatWrap a b = a +# b
  satAdd SatZero a b =
    let r = plus# a b
    in  if msb# r == low
           then truncateB# r
           else minBound#
  satAdd _ a b =
    let r  = plus# a b
    in  if msb# r == low
           then truncateB# r
           else maxBound#

  satSub SatWrap a b = a -# b
  satSub _ a b =
    let r = minus# a b
    in  if msb# r == low
           then truncateB# r
           else minBound#

  satMul SatWrap a b = a *# b
  satMul SatZero a b =
    let r       = times# a b
        (rL,rR) = split# r
    in  case rL of
          0 -> rR
          _ -> minBound#
  satMul _ a b =
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


-- error for infix operator
undefErrorI :: (HasCallStack, KnownNat m, KnownNat n) => String -> BitVector m -> BitVector n -> a
undefErrorI op bv1 bv2 = withFrozenCallStack $
  errorX $ "Clash.Sized.BitVector." ++ op
  ++ " called with (partially) undefined arguments: "
  ++ show bv1 ++ " " ++ op ++" " ++ show bv2

-- error for prefix operator/function
undefErrorP :: (HasCallStack, KnownNat m, KnownNat n) => String -> BitVector m -> BitVector n -> a
undefErrorP op bv1 bv2 = withFrozenCallStack $
  errorX $ "Clash.Sized.BitVector." ++ op
  ++ " called with (partially) undefined arguments: "
  ++ show bv1 ++ " " ++ show bv2

-- error for prefix operator/function
undefErrorP3 :: (HasCallStack, KnownNat m, KnownNat n, KnownNat o) => String -> BitVector m -> BitVector n -> BitVector o -> a
undefErrorP3 op bv1 bv2 bv3 = withFrozenCallStack $
  errorX $ "Clash.Sized.BitVector." ++ op
  ++ " called with (partially) undefined arguments: "
  ++ show bv1 ++ " " ++ show bv2 ++ " " ++ show bv3

-- error for unary operator/function
undefErrorU :: (HasCallStack, KnownNat n) => String -> BitVector n -> a
-- undefErrorU op bv1 = undefError ("Clash.Sized.BitVector." ++ op) [bv1]
undefErrorU op bv1 = withFrozenCallStack $
  errorX $ "Clash.Sized.BitVector." ++ op
  ++ " called with (partially) undefined argument: "
  ++ show bv1

undefError :: (HasCallStack, KnownNat n) => String -> [BitVector n] -> a
undefError op bvs = withFrozenCallStack $
  errorX $ op
  ++ " called with (partially) undefined arguments: "
  ++ unwords (L.map show bvs)


-- | Implement BitVector undefinedness checking for unpack funtions
checkUnpackUndef :: (KnownNat n, Typeable a)
                 => (BitVector n -> a) -- ^ unpack function
                 -> BitVector n -> a
checkUnpackUndef f bv@(BV 0 _) = f bv
checkUnpackUndef _ bv = res
  where
    ty = typeOf res
    res = undefError (show ty ++ ".unpack") [bv]
{-# NOINLINE checkUnpackUndef #-}

-- | Create a BitVector with all its bits undefined
undefined# :: forall n . KnownNat n => BitVector n
undefined# =
  let m = 1 `shiftL` fromInteger (natVal (Proxy @n))
  in  BV (m-1) 0
{-# NOINLINE undefined# #-}

-- | Check if one BitVector is like another.
-- NFDataX bits in the second argument are interpreted as don't care bits.
--
-- >>> let expected = $$(bLit "1.") :: BitVector 2
-- >>> let checked  = $$(bLit "11") :: BitVector 2
-- >>> checked  `isLike` expected
-- True
-- >>> expected `isLike` checked
-- False
--
-- __NB__: Not synthesizable
isLike :: forall n . KnownNat n => BitVector n -> BitVector n -> Bool
isLike =
  \(BV cMask c) (BV eMask e) ->
        -- set don't care bits to 0
    let e' = e .&. complementN eMask
        -- checked with undefined bits set to 0
        c' = (c .&. complementN cMask) .&. complementN eMask
        -- checked with undefined bits set to 1
        c'' = (c .|. cMask) .&. complementN eMask
    in  e' == c' && e' == c''
 where
  complementN = complementMod (natVal (Proxy @n))
{-# NOINLINE isLike #-}

fromBits :: [Bit] -> Integer
fromBits = L.foldl (\v b -> v `shiftL` 1 .|. fromIntegral b) 0

-- | Template Haskell macro for generating a pattern matching on some
-- bits of a value.
--
-- This macro compiles to an efficient view pattern that matches the
-- bits of a given value against the bits specified in the
-- pattern. The scrutinee can be any type that is an instance of the
-- 'Num', 'Bits' and 'Eq' typeclasses.
--
-- The bit pattern is specified by a string which contains @\'0\'@ or
-- @\'1\'@ for matching a bit, or @\'.\'@ for bits which are not matched.
--
-- The following example matches a byte against two bit patterns where
-- some bits are relevant and others are not:
--
-- @
--   decode :: Unsigned 8 -> Maybe Bool
--   decode $(bitPattern "00...110") = Just True
--   decode $(bitPattern "10..0001") = Just False
--   decode _ = Nothing
-- @
bitPattern :: String -> Q Pat
bitPattern s = [p| (($mask .&.) -> $target) |]
  where
    bs = parse <$> s

    mask = litE . IntegerL . fromBits $ maybe 0 (const 1) <$> bs
    target = litP . IntegerL . fromBits $ fromMaybe 0 <$> bs

    parse '.' = Nothing
    parse '0' = Just 0
    parse '1' = Just 1
    parse c = error $ "Invalid bit pattern: " ++ show c
