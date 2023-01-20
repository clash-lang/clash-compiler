{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2016     , Myrtle Software Ltd,
                  2021-2022, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RoleAnnotations #-}
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
    -- ** Enum
  , toEnum#
  , fromEnum#
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
import Text.Printf                    (PrintfArg (..), printf)
import GHC.Generics                   (Generic)
import GHC.Natural                    (naturalFromInteger, naturalToInteger)

import GHC.TypeLits                   (KnownNat, Nat, type (+), natVal)
import GHC.TypeLits.Extra             (Max)
import Data.Ix                        (Ix(..))
import Language.Haskell.TH            (appT, conT, litT, numTyLit, sigE)
import Language.Haskell.TH.Syntax     (Lift(..))
#if MIN_VERSION_template_haskell(2,16,0)
import Language.Haskell.TH.Compat
#endif
#if MIN_VERSION_template_haskell(2,17,0)
import Language.Haskell.TH            (Quote, Type)
#else
import Language.Haskell.TH            (TypeQ)
#endif
import Test.QuickCheck.Arbitrary      (Arbitrary (..), CoArbitrary (..),
                                       arbitraryBoundedIntegral,
                                       coarbitraryIntegral, shrinkIntegral)

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Class.BitPack            (BitPack (..), packXWith)
import Clash.Class.Num                (ExtendingNum (..), SaturatingNum (..),
                                       SaturationMode (..))
import Clash.Class.Parity             (Parity (..))
import Clash.Class.Resize             (Resize (..))
import Clash.Class.BitPack.BitIndex   ((!), msb, replaceBit, split)
import Clash.Class.BitPack.BitReduction (reduceAnd, reduceOr)
import Clash.Promoted.Nat             (natToNatural)
import Clash.Sized.Internal.BitVector (BitVector (BV), Bit, (++#), high, low, undefError)
import qualified Clash.Sized.Internal.BitVector as BV
import Clash.XException
  (ShowX (..), NFDataX (..), errorX, showsPrecXWith, rwhnfX)

{- $setup
>>> :m -Prelude
>>> import Clash.Prelude
-}

type role Signed nominal

-- | Arbitrary-width signed integer represented by @n@ bits, including the sign
-- bit
--
-- Uses standard 2-complements representation. Meaning that, given @n@ bits,
-- a 'Signed' @n@ number has a range of: [-(2^(@n@-1)) .. 2^(@n@-1)-1] for
-- @n > 0@. When @n = 0@, both the min and max bound are 0.
--
-- * __NB__: The usual Haskell method of converting an integral numeric type to
-- another, 'fromIntegral', is not well suited for Clash as it will go through
-- 'Integer' which is arbitrarily bounded in HDL. Instead use
-- 'Clash.Class.BitPack.bitCoerce' and the 'Resize' class.
-- * __NB__: The 'Num' operators perform @wrap-around@ on overflow. If you want
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
--
-- Signed has the <https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/roles.html type role>
--
-- >>> :i Signed
-- type role Signed nominal
-- ...
--
-- as it is not safe to coerce between different width Signed. To change the
-- width, use the functions in the 'Clash.Class.Resize.Resize' class.
#if MIN_VERSION_base(4,15,0)
data Signed (n :: Nat) =
    -- | The constructor, 'S', and the field, 'unsafeToInteger', are not
    -- synthesizable.
    S { unsafeToInteger :: !Integer}
#else
newtype Signed (n :: Nat) =
    -- | The constructor, 'S', and the field, 'unsafeToInteger', are not
    -- synthesizable.
    S { unsafeToInteger :: Integer}
#endif
  deriving (Data, Generic)

{-# ANN S hasBlackBox #-}

instance NFDataX (Signed n) where
  deepErrorX = errorX
  rnfX = rwhnfX

{-# NOINLINE size# #-}
{-# ANN size# hasBlackBox #-}
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
{-# ANN pack# hasBlackBox #-}
pack# :: forall n . KnownNat n => Signed n -> BitVector n
pack# (S i) = let m = 1 `shiftL0` fromInteger (natVal (Proxy @n))
              in  if i < 0 then BV 0 (naturalFromInteger (m + i)) else BV 0 (naturalFromInteger i)

{-# NOINLINE unpack# #-}
{-# ANN unpack# hasBlackBox #-}
unpack# :: forall n . KnownNat n => BitVector n -> Signed n
unpack# (BV 0 i) =
  let m = 1 `shiftL0` fromInteger (natVal (Proxy @n) - 1)
      n = naturalToInteger i
  in  if n >= m then S (n-2*m) else S n
unpack# bv = undefError "Signed.unpack" [bv]

instance Eq (Signed n) where
  (==) = eq#
  (/=) = neq#

{-# NOINLINE eq# #-}
{-# ANN eq# hasBlackBox #-}
eq# :: Signed n -> Signed n -> Bool
eq# (S v1) (S v2) = v1 == v2

{-# NOINLINE neq# #-}
{-# ANN neq# hasBlackBox #-}
neq# :: Signed n -> Signed n -> Bool
neq# (S v1) (S v2) = v1 /= v2

instance Ord (Signed n) where
  (<)  = lt#
  (>=) = ge#
  (>)  = gt#
  (<=) = le#

lt#,ge#,gt#,le# :: Signed n -> Signed n -> Bool
{-# NOINLINE lt# #-}
{-# ANN lt# hasBlackBox #-}
lt# (S n) (S m) = n < m
{-# NOINLINE ge# #-}
{-# ANN ge# hasBlackBox #-}
ge# (S n) (S m) = n >= m
{-# NOINLINE gt# #-}
{-# ANN gt# hasBlackBox #-}
gt# (S n) (S m) = n > m
{-# NOINLINE le# #-}
{-# ANN le# hasBlackBox #-}
le# (S n) (S m) = n <= m

-- | The functions: 'enumFrom', 'enumFromThen', 'enumFromTo', and
-- 'enumFromThenTo', are not synthesizable.
instance KnownNat n => Enum (Signed n) where
  succ n
    | n == maxBound =
        error $ "'succ' was called on (" <> show @(Signed n) maxBound <> " :: "
             <> "Signed " <> show (natToNatural @n) <> ") and caused an "
             <> "overflow. Use 'satSucc' and specify a SaturationMode if you "
             <> "need other behavior."
    | otherwise = n +# fromInteger# 1

  pred n
    | n == minBound =
        error $ "'pred' was called on (" <> show @(Signed n) maxBound <> " :: "
             <> "Signed " <> show (natToNatural @n) <> ") and caused an "
             <> "underflow. Use 'satPred' and specify a SaturationMode if you "
             <> "need other behavior."
    | otherwise = n -# fromInteger# 1

  toEnum         = toEnum#
  fromEnum       = fromEnum#
  enumFrom       = enumFrom#
  enumFromThen   = enumFromThen#
  enumFromTo     = enumFromTo#
  enumFromThenTo = enumFromThenTo#

toEnum# :: forall n. KnownNat n => Int -> Signed n
toEnum# = fromInteger# . toInteger
{-# NOINLINE toEnum# #-}
{-# ANN toEnum# hasBlackBox #-}

fromEnum# :: forall n. KnownNat n => Signed n -> Int
fromEnum# = fromEnum . toInteger#
{-# NOINLINE fromEnum# #-}
{-# ANN fromEnum# hasBlackBox #-}

enumFrom# :: forall n. KnownNat n => Signed n -> [Signed n]
enumFrom# x = map (fromInteger_INLINE sz mB mask) [unsafeToInteger x .. unsafeToInteger (maxBound :: Signed n)]
  where sz   = fromInteger (natVal (Proxy @n)) - 1
        mB   = 1 `shiftL` sz
        mask = mB - 1
{-# NOINLINE enumFrom# #-}

enumFromThen# :: forall n. KnownNat n => Signed n -> Signed n -> [Signed n]
enumFromThen# x y =
  toSigneds [unsafeToInteger x, unsafeToInteger y .. unsafeToInteger bound]
 where
  bound = if x <= y then maxBound else minBound :: Signed n
  toSigneds = map (fromInteger_INLINE sz mB mask)
  sz = fromInteger (natVal (Proxy @n)) - 1
  mB = 1 `shiftL` sz
  mask = mB - 1
{-# NOINLINE enumFromThen# #-}

enumFromTo# :: forall n. KnownNat n => Signed n -> Signed n -> [Signed n]
enumFromTo# x y = map (fromInteger_INLINE sz mB mask) [unsafeToInteger x .. unsafeToInteger y]
  where sz   = fromInteger (natVal (Proxy @n)) - 1
        mB   = 1 `shiftL` sz
        mask = mB - 1
{-# NOINLINE enumFromTo# #-}

enumFromThenTo# :: forall n. KnownNat n => Signed n -> Signed n -> Signed n -> [Signed n]
enumFromThenTo# x1 x2 y = map (fromInteger_INLINE sz mB mask) [unsafeToInteger x1, unsafeToInteger x2 .. unsafeToInteger y]
  where sz   = fromInteger (natVal (Proxy @n)) - 1
        mB   = 1 `shiftL` sz
        mask = mB - 1
{-# NOINLINE enumFromThenTo# #-}


instance KnownNat n => Bounded (Signed n) where
  minBound = minBound#
  maxBound = maxBound#

minBound# :: forall n. KnownNat n => Signed n
minBound# =
  case natToNatural @n of
    0 -> 0
    n -> S (negate $ 2 ^ (n - 1))
{-# NOINLINE minBound# #-}
{-# ANN minBound# hasBlackBox #-}

maxBound# :: forall n. KnownNat n => Signed n
maxBound# =
  case natToNatural @n of
    0 -> 0
    n -> S (2 ^ (n - 1) - 1)
{-# NOINLINE maxBound# #-}
{-# ANN maxBound# hasBlackBox #-}

-- | Operators do @wrap-around@ on overflow
--
-- __NB__: 'fromInteger'/'fromIntegral' can cause unexpected truncation, as
-- 'Integer' is arbitrarily bounded during synthesis.  Prefer
-- 'Clash.Class.BitPack.bitCoerce' and the 'Resize' class.
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
{-# ANN (+#) hasBlackBox #-}
(+#) =
  \(S a) (S b) ->
    let z = a + b
    in  if z >= m then
          S (z - 2*m)
        else if z < negate m then
          S (z + 2*m)
        else
          S z
 where
  m = 1 `shiftL0` fromInteger (natVal (Proxy @n) -1)

{-# NOINLINE (-#) #-}
{-# ANN (-#) hasBlackBox #-}
(-#) =
  \(S a) (S b) ->
    let z = a - b
    in  if z < negate m then
          S (z + 2*m)
        else if z >= m then
          S (z - 2*m)
        else
          S z
 where
  m  = 1 `shiftL0` fromInteger (natVal (Proxy @n) -1)

{-# NOINLINE (*#) #-}
{-# ANN (*#) hasBlackBox #-}
(*#) = \(S a) (S b) -> fromInteger_INLINE sz mB mask (a * b)
  where sz   = fromInteger (natVal (Proxy @n)) - 1
        mB   = 1 `shiftL` sz
        mask = mB - 1

negate#,abs# :: forall n . KnownNat n => Signed n -> Signed n
{-# NOINLINE negate# #-}
{-# ANN negate# hasBlackBox #-}
negate# =
  \(S n) ->
    let z = negate n
    in  if z == m then S n else S z
 where
  m = 1 `shiftL0` fromInteger (natVal (Proxy @n) -1)

{-# NOINLINE abs# #-}
{-# ANN abs# hasBlackBox #-}
abs# =
  \(S n) ->
    let z = abs n
    in  if z == m then S n else S z
 where
  m = 1 `shiftL0` fromInteger (natVal (Proxy @n) -1)

{-# NOINLINE fromInteger# #-}
{-# ANN fromInteger# hasBlackBox #-}
fromInteger# :: forall n . KnownNat n => Integer -> Signed (n :: Nat)
fromInteger# = fromInteger_INLINE sz mB mask
  where sz   = fromInteger (natVal (Proxy @n)) - 1
        mB   = 1 `shiftL` sz
        mask = mB - 1

{-# INLINE fromInteger_INLINE #-}
fromInteger_INLINE :: Int -> Integer -> Integer -> Integer -> Signed n
fromInteger_INLINE sz mb mask =
  \i -> let i1 = i .&. mask
            i2 = case i `shiftR` sz of
                   q | q .&. 1 == 0 -> i1
                     | otherwise    -> i1 - mb
        in  if sz < 0 then S 0 else S i2

instance ExtendingNum (Signed m) (Signed n) where
  type AResult (Signed m) (Signed n) = Signed (Max m n + 1)
  add  = plus#
  sub = minus#
  type MResult (Signed m) (Signed n) = Signed (m + n)
  mul = times#

plus#, minus# :: Signed m -> Signed n -> Signed (Max m n + 1)
{-# NOINLINE plus# #-}
{-# ANN plus# hasBlackBox #-}
plus# (S a) (S b) = S (a + b)

{-# NOINLINE minus# #-}
{-# ANN minus# hasBlackBox #-}
minus# (S a) (S b) = S (a - b)

{-# NOINLINE times# #-}
{-# ANN times# hasBlackBox #-}
times# :: Signed m -> Signed n -> Signed (m + n)
times# (S a) (S b) = S (a * b)

instance KnownNat n => Real (Signed n) where
  toRational = toRational . toInteger#

-- | __NB__: 'toInteger'/'fromIntegral' can cause unexpected truncation, as
-- 'Integer' is arbitrarily bounded during synthesis.  Prefer
-- 'Clash.Class.BitPack.bitCoerce' and the 'Resize' class.
instance KnownNat n => Integral (Signed n) where
  quot        = quot#
  rem         = rem#
  div         = div#
  mod         = mod#
  quotRem n d = (n `quot#` d,n `rem#` d)
  divMod  n d = (n `div#`  d,n `mod#` d)
  toInteger   = toInteger#

{-# NOINLINE quot# #-}
{-# ANN quot# hasBlackBox #-}
quot# :: forall n. KnownNat n => Signed n -> Signed n -> Signed n
quot# (S a) (S b)
  | a == minB && b == (-1) = S minB
  | otherwise = S (a `quot` b)
 where
  S minB = minBound @(Signed n)

{-# NOINLINE rem# #-}
{-# ANN rem# hasBlackBox #-}
rem# :: Signed n -> Signed n -> Signed n
rem# (S a) (S b) = S (a `rem` b)

{-# NOINLINE div# #-}
{-# ANN div# hasBlackBox #-}
div# :: forall n. KnownNat n => Signed n -> Signed n -> Signed n
div# (S a) (S b)
  | a == minB && b == (-1) = S minB
  | otherwise = S (a `div` b)
 where
  S minB = minBound @(Signed n)

{-# NOINLINE mod# #-}
{-# ANN mod# hasBlackBox #-}
mod# :: Signed n -> Signed n -> Signed n
mod# (S a) (S b) = S (a `mod` b)

{-# NOINLINE toInteger# #-}
{-# ANN toInteger# hasBlackBox #-}
toInteger# :: Signed n -> Integer
toInteger# (S n) = n

instance KnownNat n => PrintfArg (Signed n) where
  formatArg = formatArg . toInteger

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

and#,or#,xor# :: forall n . KnownNat n => Signed n -> Signed n -> Signed n
{-# NOINLINE and# #-}
{-# ANN and# hasBlackBox #-}
and# = \(S a) (S b) -> fromInteger_INLINE sz mB mask (a .&. b)
  where sz   = fromInteger (natVal (Proxy @n)) - 1
        mB   = 1 `shiftL` sz
        mask = mB - 1

{-# NOINLINE or# #-}
{-# ANN or# hasBlackBox #-}
or# = \(S a) (S b) -> fromInteger_INLINE sz mB mask (a .|. b)
  where sz   = fromInteger (natVal (Proxy @n)) - 1
        mB   = 1 `shiftL` sz
        mask = mB - 1

{-# NOINLINE xor# #-}
{-# ANN xor# hasBlackBox #-}
xor# = \(S a) (S b) -> fromInteger_INLINE sz mB mask (xor a b)
  where sz   = fromInteger (natVal (Proxy @n)) - 1
        mB   = 1 `shiftL` sz
        mask = mB - 1

{-# NOINLINE complement# #-}
{-# ANN complement# hasBlackBox #-}
complement# :: forall n . KnownNat n => Signed n -> Signed n
complement# = \(S a) -> fromInteger_INLINE sz mB mask (complement a)
  where sz   = fromInteger (natVal (Proxy @n)) - 1
        mB   = 1 `shiftL` sz
        mask = mB - 1

shiftL#,shiftR#,rotateL#,rotateR# :: forall n . KnownNat n => Signed n -> Int -> Signed n
{-# NOINLINE shiftL# #-}
{-# ANN shiftL# hasBlackBox #-}
shiftL# = \(S n) b ->
  if | b < 0     -> error $ "'shiftL' undefined for negative number: " ++ show b
     | b > sz    -> S 0
     | otherwise -> fromInteger_INLINE sz mB mask (shiftL n b)
 where
  sz   = fromInteger (natVal (Proxy @n)) - 1
  mB   = 1 `shiftL` sz
  mask = mB - 1

{-# NOINLINE shiftR# #-}
{-# ANN shiftR# hasBlackBox #-}
shiftR# =
  \(S n) b ->
    if b >= 0 then
      fromInteger_INLINE sz mB mask (shiftR n b)
    else
      error $ "'shiftR' undefined for negative number: " ++ show b
 where
  sz   = fromInteger (natVal (Proxy @n)) - 1
  mB   = 1 `shiftL` sz
  mask = mB - 1

{-# NOINLINE rotateL# #-}
{-# ANN rotateL# hasBlackBox #-}
rotateL# =
  \(S n) b ->
    if b >= 0 then
      let l    = shiftL n b'
          r    = shiftR n b'' .&. mask
          mask = 2 ^ b' - 1

          b'   = b `mod` sz
          b''  = sz - b'
      in  fromInteger_INLINE sz1 mB maskM (l .|. r)
    else
      error $ "'rotateL undefined for negative number: " ++ show b
 where
  sz    = fromInteger (natVal (Proxy @n))
  sz1   = sz-1
  mB    = 1 `shiftL` sz1
  maskM = mB - 1

{-# NOINLINE rotateR# #-}
{-# ANN rotateR# hasBlackBox #-}
rotateR# =
  \(S n) b ->
    if b >= 0 then
      let l    = shiftR n b' .&. mask
          r    = shiftL n b''
          mask = 2 ^ b'' - 1

          b'  = b `mod` sz
          b'' = sz - b'
      in  fromInteger_INLINE sz1 mB maskM (l .|. r)
    else
      error $ "'rotateR' undefined for negative number: " ++ show b
 where
  sz    = fromInteger (natVal (Proxy @n))
  sz1   = sz - 1
  mB    = 1 `shiftL` sz1
  maskM = mB - 1

instance KnownNat n => FiniteBits (Signed n) where
  finiteBitSize        = size#
  countLeadingZeros  s = countLeadingZeros  (pack# s)
  countTrailingZeros s = countTrailingZeros (pack# s)

instance Resize Signed where
  resize       = resize#
  zeroExtend s = unpack# (0 ++# pack s)
  truncateB    = truncateB#

{-# NOINLINE resize# #-}
{-# ANN resize# hasBlackBox #-}
resize# :: forall m n . (KnownNat n, KnownNat m) => Signed n -> Signed m
resize# s@(S i)
  | natToNatural @m == 0 = S 0
  | n' <= m'  = extended
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
{-# ANN truncateB# hasBlackBox #-}
truncateB# :: forall m n . KnownNat m => Signed (m + n) -> Signed m
truncateB# = \(S n) -> fromInteger_INLINE sz mB mask n
  where sz   = fromInteger (natVal (Proxy @m)) - 1
        mB   = 1 `shiftL` sz
        mask = mB - 1

instance KnownNat n => Default (Signed n) where
  def = fromInteger# 0

instance KnownNat n => Lift (Signed n) where
  lift s@(S i) = sigE [| fromInteger# i |] (decSigned (natVal s))
  {-# NOINLINE lift #-}
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = liftTypedFromUntyped
#endif

#if MIN_VERSION_template_haskell(2,17,0)
decSigned :: Quote m => Integer -> m Type
#else
decSigned :: Integer -> TypeQ
#endif
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
  satAdd SatError a b =
    let r      = plus# a b
        (_,r') = split r
    in  case msb r `xor` msb r' of
          0 -> unpack# r'
          _ -> errorX "Signed.satAdd: overflow/underflow"
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
  satSub SatError a b =
    let r      = minus# a b
        (_,r') = split r
    in  case msb r `xor` msb r' of
          0 -> unpack# r'
          _ -> errorX "Signed.satSub: overflow/underflow"
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
  satMul SatError a b =
    let r        = times# a b
        (rL,rR)  = split r
        overflow = complement (reduceOr (BV.pack# (msb rR) ++# pack rL)) .|.
                   reduceAnd (BV.pack# (msb rR) ++# pack rL)
    in  case overflow of
          1 -> unpack# rR
          _ -> errorX "Signed.satMul: overflow/underflow"
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

  satSucc SatError a
    | a == maxBound = errorX "Signed.satSucc: overflow"
  satSucc satMode a = satSub satMode a $ fromInteger# (-1)
  {-# INLINE satSucc #-}

  satPred SatError a
    | a == minBound = errorX "Signed.satPred: underflow"
  satPred satMode a = satAdd satMode a $ fromInteger# (-1)
  {-# INLINE satPred #-}

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

instance (KnownNat n) => Ix (Signed n) where
  range (a, b) = [a..b]
  index ab@(a, b) x
    | inRange ab x = fromIntegral $ x - a
    | otherwise = error $ printf "Index %d out of bounds (%d, %d) ab" x a b
  inRange (a, b) x = a <= x && x <= b

-- | Shift left that ties to zero on negative shifts
shiftL0 :: Integer -> Int -> Integer
#if MIN_VERSION_base(4,15,0)
shiftL0 = \a sh -> if sh >= 0 then shiftL a sh else 0
#else
shiftL0 = shiftL -- True for use with this module
#endif
{-# INLINE shiftL0 #-}
