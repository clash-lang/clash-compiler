{-|
Copyright  :  (C) 2013-2016, University of Twente,
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

module Clash.Sized.Internal.Index
  ( -- * Datatypes
    Index (..)
    -- * Construction
  , fromSNat
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
  , maxBound#
    -- ** Num
  , (+#)
  , (-#)
  , (*#)
  , fromInteger#
    -- ** ExtendingNum
  , plus#
  , minus#
  , times#
    -- ** Integral
  , quot#
  , rem#
  , toInteger#
    -- ** Resize
  , resize#
  )
where

import Prelude hiding             (even, odd)

import Control.DeepSeq            (NFData (..))
import Data.Bits                  (Bits (..), FiniteBits (..))
import Data.Data                  (Data)
import Data.Default.Class         (Default (..))
import Data.Proxy                 (Proxy (..))
import Text.Read                  (Read (..), ReadPrec)
import Language.Haskell.TH        (TypeQ, appT, conT, litT, numTyLit, sigE)
import Language.Haskell.TH.Syntax (Lift(..))
import GHC.Generics               (Generic)
import GHC.Natural                (Natural, naturalFromInteger)
#if MIN_VERSION_base(4,12,0)
import GHC.Natural                (naturalToInteger)
#else
import Clash.Sized.Internal.Mod   (naturalToInteger)
#endif
import GHC.Stack                  (HasCallStack)
import GHC.TypeLits               (KnownNat, Nat, type (+), type (-),
                                   type (*), type (<=), natVal)
import GHC.TypeLits.Extra         (CLog)
import Test.QuickCheck.Arbitrary  (Arbitrary (..), CoArbitrary (..),
                                   arbitraryBoundedIntegral,
                                   coarbitraryIntegral, shrinkIntegral)

import Clash.Class.BitPack        (BitPack (..), packXWith)
import Clash.Class.Num            (ExtendingNum (..), SaturatingNum (..),
                                   SaturationMode (..))
import Clash.Class.Parity         (Parity (..))
import Clash.Class.Resize         (Resize (..))
import Clash.Prelude.BitIndex     (replaceBit)
import {-# SOURCE #-} Clash.Sized.Internal.BitVector (BitVector (BV), high, low, undefError)
import qualified Clash.Sized.Internal.BitVector as BV
import Clash.Promoted.Nat         (SNat(..), snatToNum, natToInteger, leToPlusKN)
import Clash.XException
  (ShowX (..), NFDataX (..), errorX, showsPrecXWith, rwhnfX)

{- $setup
>>> import Clash.Sized.Internal.Index
-}

-- | Arbitrary-bounded unsigned integer represented by @ceil(log_2(n))@ bits.
--
-- Given an upper bound @n@, an 'Index' @n@ number has a range of: [0 .. @n@-1]
--
-- >>> maxBound :: Index 8
-- 7
-- >>> minBound :: Index 8
-- 0
-- >>> read (show (maxBound :: Index 8)) :: Index 8
-- 7
-- >>> 1 + 2 :: Index 8
-- 3
-- >>> 2 + 6 :: Index 8
-- *** Exception: X: Clash.Sized.Index: result 8 is out of bounds: [0..7]
-- ...
-- >>> 1 - 3 :: Index 8
-- *** Exception: X: Clash.Sized.Index: result -2 is out of bounds: [0..7]
-- ...
-- >>> 2 * 3 :: Index 8
-- 6
-- >>> 2 * 4 :: Index 8
-- *** Exception: X: Clash.Sized.Index: result 8 is out of bounds: [0..7]
-- ...
newtype SatIndex (sat :: SaturationMode) (n :: Nat) =
  -- | The constructor, 'I', and the field, 'unsafeToInteger', are not
  -- synthesizable.
  I { unsafeToInteger :: Integer }
  deriving (Data, Generic)

type Index n = SatIndex 'SatUnsafe n

{-# NOINLINE size# #-}
size# :: (KnownNat n, 1 <= n) => SatIndex sat n -> Int
size# = BV.size# . pack#

instance NFData (SatIndex sat n) where
  rnf (I i) = rnf i `seq` ()
  {-# NOINLINE rnf #-}
-- NOINLINE is needed so that Clash doesn't trip on the "Index ~# Integer"
-- coercion

instance (KnownSatMode sat, KnownNat n, 1 <= n) => BitPack (SatIndex sat n) where
  type BitSize (SatIndex sat n) = CLog 2 n
  pack   = packXWith pack#
  unpack = unpack#

-- | Safely convert an `SNat` value to an `Index`
-- TODO: Remove this function??

fromSNat
  :: (KnownSatMode sat, KnownNat n, KnownNat m, 1 <= n, 1 <= m, CmpNat n m ~ 'LT)
  => SNat n
  -> SatIndex sat m
fromSNat = snatToNum

{-# NOINLINE pack# #-}
pack# :: SatIndex sat n -> BitVector (CLog 2 n)
pack# (I i) = BV 0 (naturalFromInteger i)

{-# NOINLINE unpack# #-}
unpack# :: (KnownSatMode sat, KnownNat n, 1 <= n) => BitVector (CLog 2 n) -> SatIndex sat n
unpack# (BV 0 i) = fromInteger_INLINE (naturalToInteger i)
unpack# bv = undefError "Index.unpack" [bv]

instance Eq (SatIndex sat n) where
  (==) = eq#
  (/=) = neq#

{-# NOINLINE eq# #-}
eq# :: (SatIndex sat1 n) -> (SatIndex sat2 n) -> Bool
(I n) `eq#` (I m) = n == m

{-# NOINLINE neq# #-}
neq# :: (SatIndex sat1 n) -> (SatIndex sat2 n) -> Bool
(I n) `neq#` (I m) = n /= m

instance Ord (SatIndex sat n) where
  (<)  = lt#
  (>=) = ge#
  (>)  = gt#
  (<=) = le#

lt#,ge#,gt#,le# :: SatIndex sat1 n -> SatIndex sat2 n -> Bool
{-# NOINLINE lt# #-}
lt# (I n) (I m) = n < m
{-# NOINLINE ge# #-}
ge# (I n) (I m) = n >= m
{-# NOINLINE gt# #-}
gt# (I n) (I m) = n > m
{-# NOINLINE le# #-}
le# (I n) (I m) = n <= m

-- | The functions: 'enumFrom', 'enumFromThen', 'enumFromTo', and
-- 'enumFromThenTo', are not synthesizable.
instance KnownNat n => Enum (SatIndex sat n) where
  succ           = (+# fromInteger# 1)
  pred           = (-# fromInteger# 1)
  toEnum         = fromInteger# . toInteger
  fromEnum       = fromEnum . toInteger#
  enumFrom       = enumFrom#
  enumFromThen   = enumFromThen#
  enumFromTo     = enumFromTo#
  enumFromThenTo = enumFromThenTo#

enumFrom# :: forall n. KnownNat n => Index n -> [Index n]
enumFrom# x = [x .. maxBound]
{-# NOINLINE enumFrom# #-}

enumFromThen# :: forall n. KnownNat n => Index n -> Index n -> [Index n]
enumFromThen# x y = if x <= y then [x, y .. maxBound] else [x, y .. minBound]
{-# NOINLINE enumFromThen# #-}

enumFromTo# :: Index n -> Index n -> [Index n]
enumFromTo# x y = map I [unsafeToInteger x .. unsafeToInteger y]
{-# NOINLINE enumFromTo# #-}
{-# NOINLINE enumFromThenTo# #-}
enumFrom#       :: forall n. KnownNat n => SatIndex sat n -> [SatIndex sat n]
enumFromThen#   :: forall n. KnownNat n => SatIndex sat n -> SatIndex sat n -> [SatIndex sat n]
enumFromTo#     :: SatIndex sat n -> SatIndex sat n -> [SatIndex sat n]
enumFromThenTo# :: SatIndex sat n -> SatIndex sat n -> SatIndex sat n -> [SatIndex sat n]
enumFrom# x             = map fromInteger_INLINE [unsafeToInteger x .. unsafeToInteger (maxBound :: SatIndex sat n)]
enumFromThen# x y       = map fromInteger_INLINE [unsafeToInteger x, unsafeToInteger y .. unsafeToInteger (maxBound :: SatIndex sat n)]
enumFromTo# x y         = map I [unsafeToInteger x .. unsafeToInteger y]
enumFromThenTo# x1 x2 y = map I [unsafeToInteger x1, unsafeToInteger x2 .. unsafeToInteger y]
{-# NOINLINE enumFromThenTo# #-}

instance KnownNat n => Bounded (SatIndex sat n) where
  minBound = fromInteger# 0
  maxBound = maxBound#

maxBound# :: forall n. KnownNat n => SatIndex sat n
maxBound# =
  case natToInteger @n of
    0 -> errorX "maxBound of 'Index 0' is undefined"
    n -> fromInteger_INLINE (n - 1)
{-# NOINLINE maxBound# #-}

-- | Operators report an error on overflow and underflow
instance (KnownSatMode sat, KnownNat n, 1 <= n) => Num (SatIndex sat n) where
  (+)         = satAdd $ satModeVal $ Proxy @sat
  (-)         = satSub $ satModeVal $ Proxy @sat
  (*)         = satMul $ satModeVal $ Proxy @sat
  negate      = (maxBound# -#)
  abs         = id
  signum i    = if i == 0 then 0 else 1
  fromInteger = fromInteger#

fromInteger# :: KnownNat n => Integer -> SatIndex sat n
{-# NOINLINE fromInteger# #-}
fromInteger# = fromInteger_INLINE
{-# INLINE fromInteger_INLINE #-}
fromInteger_INLINE :: forall n sat. (HasCallStack, KnownNat n) => Integer -> SatIndex sat n
fromInteger_INLINE i = bound `seq` if i > (-1) && i < bound then I i else err
  where
    bound = natVal (Proxy @n)
    err   = errorX ("Clash.Sized.Index: result " ++ show i ++
                   " is out of bounds: [0.." ++ show (bound - 1) ++ "]")


instance ExtendingNum (SatIndex sat m) (SatIndex sat n) where
  type AResult (SatIndex sat m) (SatIndex sat n) = SatIndex sat (m + n - 1)
  add  = plus#
  sub = minus#
  type MResult (SatIndex sat m) (SatIndex sat n) = SatIndex sat (((m - 1) * (n - 1)) + 1)
  mul = times#

plus#, minus# :: SatIndex sat m -> SatIndex sat n -> SatIndex sat (m + n - 1)
{-# NOINLINE plus# #-}
plus# (I a) (I b) = I (a + b)

{-# NOINLINE minus# #-}
minus# (I a) (I b) =
  let z   = a - b
      err = error ("Clash.Sized.Index.minus: result " ++ show z ++
                   " is smaller than 0")
      res = if z < 0 then err else I z
  in  res

{-# NOINLINE times# #-}
times# :: SatIndex sat m -> SatIndex sat n -> SatIndex sat (((m - 1) * (n - 1)) + 1)
times# (I a) (I b) = I (a * b)

instance (KnownSatMode sat, KnownNat n, 1 <= n) => SaturatingNum (SatIndex sat n) where
  satAdd SatWrap !a !b =
    case snatToNum @Integer (SNat @n) of
      1 -> fromInteger# 0
      _ -> leToPlusKN @1 @n $
        case plus# a b of
          z | let m = fromInteger# (natVal (Proxy @ n))
            , z >= m -> resize# (z - m)
          z -> resize# z
  satAdd SatUnsafe a b = a +# b
  satAdd _ a b =
    leToPlusKN @1 @n $
      case plus# a b of
        z | let m = fromInteger# (natVal (Proxy @ (n - 1)))
          , z > m -> maxBound#
        z -> resize# z

  satSub SatWrap a b =
    if lt# a b
       then maxBound -# (b -# a) +# 1
       else a -# b
  satSub SatUnsafe a b = a -# b
  satSub _ a b =
    if lt# a b
       then fromInteger# 0
       else a -# b

  satMul SatWrap !a !b =
    case snatToNum @Integer (SNat @n) of
      1 -> fromInteger# 0
      _ -> leToPlusKN @1 @n $
        case times# a b of
          z -> let m = fromInteger# (natVal (Proxy @ n))
               in resize# (z `mod` m)
  satMul SatZero a b =
    leToPlusKN @1 @n $
      case times# a b of
        z | let m = fromInteger# (natVal (Proxy @ (n - 1)))
          , z > m -> fromInteger# 0
        z -> resize# z
  satMul SatUnsafe a b = a *# b
  satMul _ a b =
    leToPlusKN @1 @n $
      case times# a b of
        z | let m = fromInteger# (natVal (Proxy @ (n - 1)))
          , z > m -> maxBound#
        z -> resize# z

(+#),(-#),(*#) :: KnownNat n => SatIndex sat n -> SatIndex sat n -> SatIndex sat n
{-# NOINLINE (+#) #-}
(+#) (I a) (I b) = fromInteger_INLINE $ a + b

{-# NOINLINE (-#) #-}
(-#) (I a) (I b) = fromInteger_INLINE $ a - b

{-# NOINLINE (*#) #-}
(*#) (I a) (I b) = fromInteger_INLINE $ a * b

instance (KnownSatMode sat, KnownNat n, 1 <= n) => Real (SatIndex sat n) where
  toRational = toRational . toInteger#

instance (KnownSatMode sat, KnownNat n, 1 <= n) => Integral (SatIndex sat n) where
  quot        = quot#
  rem         = rem#
  div         = quot#
  mod         = rem#
  quotRem n d = (n `quot#` d,n `rem#` d)
  divMod  n d = (n `quot#` d,n `rem#` d)
  toInteger   = toInteger#

quot#,rem# :: SatIndex sat n -> SatIndex sat n -> SatIndex sat n
{-# NOINLINE quot# #-}
(I a) `quot#` (I b) = I (a `div` b)
{-# NOINLINE rem# #-}
(I a) `rem#` (I b) = I (a `rem` b)

{-# NOINLINE toInteger# #-}
toInteger# :: SatIndex sat n -> Integer
toInteger# (I n) = n

instance (KnownSatMode sat, KnownNat n, 1 <= n) => Parity (SatIndex sat n) where
  even = even . pack
  odd = odd . pack

instance (KnownSatMode sat, KnownNat n, 1 <= n) => Bits (SatIndex sat n) where
  a .&. b           = unpack# $ BV.and# (pack# a) (pack# b)
  a .|. b           = unpack# $ BV.or# (pack# a) (pack# b)
  xor a b           = unpack# $ BV.xor# (pack# a) (pack# b)
  complement        = unpack# . BV.complement# . pack#
  zeroBits          = unpack# zeroBits
  bit i             = unpack# $ bit i
  setBit v i        = unpack# $ replaceBit i high (pack# v)
  clearBit v i      = unpack# $ replaceBit i low  (pack# v)
  complementBit v i = unpack# $ complementBit (pack# v) i
  testBit v i       = testBit (pack# v) i
  bitSizeMaybe v    = Just (size# v)
  bitSize           = size#
  isSigned _        = False
  shiftL v i        = unpack# $ shiftL (pack# v) i
  shiftR v i        = unpack# $ shiftR (pack# v) i
  rotateL v i       = unpack# $ rotateL (pack# v) i
  rotateR v i       = unpack# $ rotateR (pack# v) i
  popCount i        = popCount (pack# i)

instance (KnownSatMode sat, KnownNat n, 1 <= n) => FiniteBits (SatIndex sat n) where
  finiteBitSize        = size#
  countLeadingZeros  i = countLeadingZeros  (pack# i)
  countTrailingZeros i = countTrailingZeros (pack# i)

instance Resize (SatIndex sat) where
  resize     = resize#
  zeroExtend = extend
  truncateB  = resize#

resize# :: KnownNat m => SatIndex sat n -> SatIndex sat m
resize# (I i) = fromInteger_INLINE i
{-# NOINLINE resize# #-}

instance KnownNat n => Lift (SatIndex sat n) where
  lift u@(I i) = sigE [| fromInteger# i |] (decIndex (natVal u))
  {-# NOINLINE lift #-}

decIndex :: Integer -> TypeQ
decIndex n = appT (conT ''SatIndex) (litT $ numTyLit n)

instance Show (SatIndex sat n) where
  show (I i) = show i
  {-# NOINLINE show #-}

instance ShowX (SatIndex sat n) where
  showsPrecX = showsPrecXWith showsPrec

instance NFDataX (SatIndex sat n) where
  deepErrorX = errorX
  rnfX = rwhnfX

-- | None of the 'Read' class' methods are synthesizable.
instance (KnownSatMode sat, KnownNat n, 1 <= n) => Read (SatIndex sat n) where
  readPrec = fromIntegral <$> (readPrec :: ReadPrec Natural)

instance KnownNat n => Default (SatIndex sat n) where
  def = fromInteger# 0

instance (KnownSatMode sat, KnownNat n, 1 <= n) => Arbitrary (SatIndex sat n) where
  arbitrary = arbitraryBoundedIntegral
  shrink    = shrinkIndex

shrinkIndex :: (KnownSatMode sat, KnownNat n, 1 <= n) => SatIndex sat n -> [SatIndex sat n]
shrinkIndex x | natVal x < 3 = case toInteger x of
                                 1 -> [0]
                                 _ -> []
              -- 'shrinkIntegral' uses "`quot` 2", which for 'Index' types with
              -- an upper bound less than 2 results in an error.
              | otherwise    = shrinkIntegral x

instance (KnownSatMode sat, KnownNat n, 1 <= n) => CoArbitrary (SatIndex sat n) where
  coarbitrary = coarbitraryIntegral
