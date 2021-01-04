{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2016     , Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP #-}
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
import Text.Printf                    (PrintfArg (..), printf)
import GHC.Generics                   (Generic)
import GHC.Natural                    (naturalFromInteger)
#if MIN_VERSION_base(4,12,0)
import GHC.Natural                    (naturalToInteger)
#else
import Clash.Sized.Internal.Mod       (naturalToInteger)
#endif

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

import Clash.Class.BitPack            (BitPack (..), packXWith)
import Clash.Class.Num                (ExtendingNum (..), SaturatingNum (..),
                                       SaturationMode (..))
import Clash.Class.Parity             (Parity (..))
import Clash.Class.Resize             (Resize (..))
import Clash.Prelude.BitIndex         ((!), msb, replaceBit, split)
import Clash.Prelude.BitReduction     (reduceAnd, reduceOr)
import Clash.Promoted.Nat             (natToNatural)
import Clash.Sized.Internal.BitVector (BitVector (BV), Bit, (++#), high, low, undefError)
import qualified Clash.Sized.Internal.BitVector as BV
import Clash.XException
  (ShowX (..), NFDataX (..), errorX, showsPrecXWith, rwhnfX)

-- | Arbitrary-width signed integer represented by @n@ bits, including the sign
-- bit.
--
-- Uses standard 2-complements representation. Meaning that, given @n@ bits,
-- a 'Signed' @n@ number has a range of: [-(2^(@n@-1)) .. 2^(@n@-1)-1] for
-- @n > 0@. When @n = 0@, both the min and max bound are 0.
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
pack# (S i) = let m = 1 `shiftL0` fromInteger (natVal (Proxy @n))
              in  if i < 0 then BV 0 (naturalFromInteger (m + i)) else BV 0 (naturalFromInteger i)

{-# NOINLINE unpack# #-}
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

  toEnum         = fromInteger# . toInteger
  fromEnum       = fromEnum . toInteger#
  enumFrom       = enumFrom#
  enumFromThen   = enumFromThen#
  enumFromTo     = enumFromTo#
  enumFromThenTo = enumFromThenTo#


enumFrom# :: forall n. KnownNat n => Signed n -> [Signed n]
enumFrom# x = map (fromInteger_INLINE sz mB mask) [unsafeToInteger x .. unsafeToInteger (maxBound :: Signed n)]
  where sz   = fromInteger (natVal (Proxy @n)) - 1
        mB   = 1 `shiftL0` sz
        mask = mB - 1
{-# NOINLINE enumFrom# #-}

enumFromThen# :: forall n. KnownNat n => Signed n -> Signed n -> [Signed n]
enumFromThen# x y =
  toSigneds [unsafeToInteger x, unsafeToInteger y .. unsafeToInteger bound]
 where
  bound = if x <= y then maxBound else minBound :: Signed n
  toSigneds = map (fromInteger_INLINE sz mB mask)
  sz = fromInteger (natVal (Proxy @n)) - 1
  mB = 1 `shiftL0` sz
  mask = mB - 1
{-# NOINLINE enumFromThen# #-}

enumFromTo# :: forall n. KnownNat n => Signed n -> Signed n -> [Signed n]
enumFromTo# x y = map (fromInteger_INLINE sz mB mask) [unsafeToInteger x .. unsafeToInteger y]
  where sz   = fromInteger (natVal (Proxy @n)) - 1
        mB   = 1 `shiftL0` sz
        mask = mB - 1
{-# NOINLINE enumFromTo# #-}

enumFromThenTo# :: forall n. KnownNat n => Signed n -> Signed n -> Signed n -> [Signed n]
enumFromThenTo# x1 x2 y = map (fromInteger_INLINE sz mB mask) [unsafeToInteger x1, unsafeToInteger x2 .. unsafeToInteger y]
  where sz   = fromInteger (natVal (Proxy @n)) - 1
        mB   = 1 `shiftL0` sz
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

maxBound# :: forall n. KnownNat n => Signed n
maxBound# =
  case natToNatural @n of
    0 -> 0
    n -> S (2 ^ (n - 1) - 1)
{-# NOINLINE maxBound# #-}

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
(*#) = \(S a) (S b) -> fromInteger_INLINE sz mB mask (a * b)
  where sz   = fromInteger (natVal (Proxy @n)) - 1
        mB   = 1 `shiftL0` sz
        mask = mB - 1

negate#,abs# :: forall n . KnownNat n => Signed n -> Signed n
{-# NOINLINE negate# #-}
negate# =
  \(S n) ->
    let z = negate n
    in  if z == m then S n else S z
 where
  m = 1 `shiftL0` fromInteger (natVal (Proxy @n) -1)

{-# NOINLINE abs# #-}
abs# =
  \(S n) ->
    let z = abs n
    in  if z == m then S n else S z
 where
  m = 1 `shiftL0` fromInteger (natVal (Proxy @n) -1)

{-# NOINLINE fromInteger# #-}
fromInteger# :: forall n . KnownNat n => Integer -> Signed (n :: Nat)
fromInteger# = fromInteger_INLINE sz mB mask
  where sz   = fromInteger (natVal (Proxy @n)) - 1
        mB   = 1 `shiftL0` sz
        mask = mB - 1

{-# INLINE fromInteger_INLINE #-}
fromInteger_INLINE :: Int -> Integer -> Integer -> Integer -> Signed n
fromInteger_INLINE sz mb mask =
  \i -> let i1 = i .&. mask
            i2 = case i `shiftR` sz of
                   q | q .&. 1 == 0 -> i1
                     | otherwise    -> i1 - mb
        in  if mb == 0 then S 0 else S i2

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
and# = \(S a) (S b) -> fromInteger_INLINE sz mB mask (a .&. b)
  where sz   = fromInteger (natVal (Proxy @n)) - 1
        mB   = 1 `shiftL` sz
        mask = mB - 1

{-# NOINLINE or# #-}
or# = \(S a) (S b) -> fromInteger_INLINE sz mB mask (a .|. b)
  where sz   = fromInteger (natVal (Proxy @n)) - 1
        mB   = 1 `shiftL` sz
        mask = mB - 1

{-# NOINLINE xor# #-}
xor# = \(S a) (S b) -> fromInteger_INLINE sz mB mask (xor a b)
  where sz   = fromInteger (natVal (Proxy @n)) - 1
        mB   = 1 `shiftL` sz
        mask = mB - 1

{-# NOINLINE complement# #-}
complement# :: forall n . KnownNat n => Signed n -> Signed n
complement# = \(S a) -> fromInteger_INLINE sz mB mask (complement a)
  where sz   = fromInteger (natVal (Proxy @n)) - 1
        mB   = 1 `shiftL` sz
        mask = mB - 1

shiftL#,shiftR#,rotateL#,rotateR# :: forall n . KnownNat n => Signed n -> Int -> Signed n
{-# NOINLINE shiftL# #-}
shiftL# =
  \(S n) b ->
    if b >= 0 then
      fromInteger_INLINE sz mB mask (shiftL n b)
    else
      error "'shiftL' undefined for negative numbers"
 where
  sz   = fromInteger (natVal (Proxy @n)) - 1
  mB   = 1 `shiftL` sz
  mask = mB - 1

{-# NOINLINE shiftR# #-}
shiftR# =
  \(S n) b ->
    if b >= 0 then
      fromInteger_INLINE sz mB mask (shiftR n b)
    else
      error "'shiftR' undefined for negative numbers"
 where
  sz   = fromInteger (natVal (Proxy @n)) - 1
  mB   = 1 `shiftL` sz
  mask = mB - 1

{-# NOINLINE rotateL# #-}
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
      error "'rotateL undefined for negative numbers"
 where
  sz    = fromInteger (natVal (Proxy @n))
  sz1   = sz-1
  mB    = 1 `shiftL` sz1
  maskM = mB - 1

{-# NOINLINE rotateR# #-}
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
      error "'rotateR' undefined for negative numbers"
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

  satSucc satMode a = satSub satMode a $ fromInteger# (-1)
  {-# INLINE satSucc #-}
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
