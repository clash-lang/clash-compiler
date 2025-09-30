{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2016     , Myrtle Software Ltd,
                  2021-2025, QBayLogic B.V.
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

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK show-extensions not-home #-}

module Clash.Sized.Internal.Unsigned
  ( -- * Datatypes
    Unsigned (..)
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
    -- ** Resize
  , resize#
    -- ** Conversions
  , unsignedToWord
  , unsigned8toWord8
  , unsigned16toWord16
  , unsigned32toWord32
  )
where

import Prelude hiding                 (even, odd)

import Control.DeepSeq                (NFData (..))
import Control.Lens                   (Index, Ixed (..), IxValue)
import Data.Bits                      (Bits (..), FiniteBits (..))
import Data.Data                      (Data)
import Data.Default                   (Default (..))
import Data.Proxy                     (Proxy (..))
import Text.Read                      (Read (..), ReadPrec)
import Text.Printf                    (PrintfArg (..), printf)
#if MIN_VERSION_base(4,16,0)
import GHC.Exts                       (wordToWord8#, wordToWord16#, wordToWord32#)
#else
import GHC.Exts                       (narrow8Word#, narrow16Word#, narrow32Word#)
#endif
import GHC.Generics                   (Generic)
#if MIN_VERSION_base(4,15,0)
import GHC.Num.BigNat                 (bigNatToWord, bigNatToWord#)
import GHC.Num.Integer
  (integerFromNatural, integerShiftL, integerToNatural)
import GHC.Num.Natural
  (Natural (..), naturalShiftL, naturalShiftR, naturalToWord)
#else
import GHC.Integer.GMP.Internals      (bigNatToWord)
import GHC.Natural                    (Natural (..), naturalFromInteger)
#endif
import GHC.Natural                    (naturalToInteger)
import GHC.TypeLits                   (KnownNat, Nat, type (+))
#if MIN_VERSION_base(4,15,0)
import GHC.TypeNats                   (natVal)
#else
import GHC.TypeLits                   (natVal)
#endif
import GHC.TypeLits.Extra             (Max)
import GHC.Word                       (Word (..), Word8 (..), Word16 (..), Word32 (..))
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
                                       coarbitraryIntegral)

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Class.BitPack            (BitPack (..), packXWith, bitCoerce)
import Clash.Class.Num                (ExtendingNum (..), SaturatingNum (..),
                                       SaturationMode (..))
import Clash.Class.Parity             (Parity (..))
import Clash.Class.Resize             (Resize (..))
import Clash.Class.BitPack.BitIndex   ((!), msb, replaceBit, split)
import Clash.Class.BitPack.BitReduction (reduceOr)
import Clash.Promoted.Nat             (natToNum, natToNatural)
import Clash.Sized.Internal.BitVector (BitVector (BV), Bit, high, low, undefError)
import qualified Clash.Sized.Internal.BitVector as BV
import Clash.Sized.Internal.Mod
import Clash.XException
  (ShowX (..), NFDataX (..), errorX, showsPrecXWith, rwhnfX)

{- $setup
>>> :m -Prelude
>>> import Clash.Prelude
-}

#include "MachDeps.h"

type role Unsigned nominal

-- | Arbitrary-width unsigned integer represented by @n@ bits
--
-- Given @n@ bits, an 'Unsigned' @n@ number has a range of: [0 .. 2^@n@-1]
--
-- * __NB__: The usual Haskell method of converting an integral numeric type to
-- another, 'fromIntegral', is not well suited for Clash as it will go through
-- 'Integer' which is arbitrarily bounded in HDL. Instead use
-- 'Clash.Class.BitPack.bitCoerce' and the 'Resize' class.
-- * __NB__: The 'Num' operators perform @wrap-around@ on overflow. If you want
-- saturation on overflow, check out the 'SaturatingNum' class.
--
-- >>> maxBound :: Unsigned 3
-- 7
-- >>> minBound :: Unsigned 3
-- 0
-- >>> read (show (maxBound :: Unsigned 3)) :: Unsigned 3
-- 7
-- >>> 1 + 2 :: Unsigned 3
-- 3
-- >>> 2 + 6 :: Unsigned 3
-- 0
-- >>> 1 - 3 :: Unsigned 3
-- 6
-- >>> 2 * 3 :: Unsigned 3
-- 6
-- >>> 2 * 4 :: Unsigned 3
-- 0
-- >>> (2 :: Unsigned 3) `mul` (4 :: Unsigned 3) :: Unsigned 6
-- 8
-- >>> (2 :: Unsigned 3) `add` (6 :: Unsigned 3) :: Unsigned 4
-- 8
-- >>> satAdd SatSymmetric 2 6 :: Unsigned 3
-- 7
-- >>> satSub SatSymmetric 2 3 :: Unsigned 3
-- 0
--
-- Unsigned has the <https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/roles.html type role>
--
-- >>> :i Unsigned
-- type role Unsigned nominal
-- ...
--
-- as it is not safe to coerce between different width Unsigned. To change the
-- width, use the functions in the 'Clash.Class.Resize.Resize' class.
data Unsigned (n :: Nat) =
    -- | The constructor, 'U', and the field, 'unsafeToNatural', are not
    -- synthesizable.
    U { unsafeToNatural :: !Natural }
  deriving (Data, Generic)

{-# ANN U hasBlackBox #-}

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE size# #-}
{-# ANN size# hasBlackBox #-}
size# :: KnownNat n => Unsigned n -> Int
#if MIN_VERSION_base(4,15,0)
size# u = fromIntegral (natVal u)
#else
size# u = fromInteger (natVal u)
#endif

instance NFData (Unsigned n) where
  rnf (U i) = rnf i `seq` ()
  {-# NOINLINE rnf #-}
  -- NOINLINE is needed so that Clash doesn't trip on the "Unsigned ~# Natural"
  -- coercion

instance Show (Unsigned n) where
  show (U i) = show i
  {-# NOINLINE show #-}

instance ShowX (Unsigned n) where
  showsPrecX = showsPrecXWith showsPrec

instance NFDataX (Unsigned n) where
  deepErrorX = errorX
  ensureSpine = id
  rnfX = rwhnfX

-- | None of the 'Read' class' methods are synthesizable.
instance KnownNat n => Read (Unsigned n) where
  readPrec = fromIntegral <$> (readPrec :: ReadPrec Natural)

instance KnownNat n => BitPack (Unsigned n) where
  type BitSize (Unsigned n) = n
  pack   = packXWith pack#
  unpack = unpack#

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE pack# #-}
{-# ANN pack# hasBlackBox #-}
pack# :: Unsigned n -> BitVector n
pack# (U i) = BV 0 i

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unpack# #-}
{-# ANN unpack# hasBlackBox #-}
unpack# :: KnownNat n => BitVector n -> Unsigned n
unpack# (BV 0 i) = U i
unpack# bv = undefError "Unsigned.unpack" [bv]

instance Eq (Unsigned n) where
  (==) = eq#
  (/=) = neq#

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE eq# #-}
{-# ANN eq# hasBlackBox #-}
eq# :: Unsigned n -> Unsigned n -> Bool
eq# (U v1) (U v2) = v1 == v2

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE neq# #-}
{-# ANN neq# hasBlackBox #-}
neq# :: Unsigned n -> Unsigned n -> Bool
neq# (U v1) (U v2) = v1 /= v2

instance Ord (Unsigned n) where
  (<)  = lt#
  (>=) = ge#
  (>)  = gt#
  (<=) = le#

lt#,ge#,gt#,le# :: Unsigned n -> Unsigned n -> Bool
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE lt# #-}
{-# ANN lt# hasBlackBox #-}
lt# (U n) (U m) = n < m
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE ge# #-}
{-# ANN ge# hasBlackBox #-}
ge# (U n) (U m) = n >= m
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE gt# #-}
{-# ANN gt# hasBlackBox #-}
gt# (U n) (U m) = n > m
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE le# #-}
{-# ANN le# hasBlackBox #-}
le# (U n) (U m) = n <= m

-- | The functions: 'enumFrom', 'enumFromThen', 'enumFromTo', and
-- 'enumFromThenTo', are not synthesizable.
instance KnownNat n => Enum (Unsigned n) where
  succ n
    | n == maxBound =
        error $ "'succ' was called on (" <> show @(Unsigned n) maxBound <> " :: "
             <> "Unsigned " <> show (natToNatural @n) <> ") and caused an "
             <> "overflow. Use 'satSucc' and specify a SaturationMode if you "
             <> "need other behavior."
    | otherwise = n +# fromInteger# 1

  pred n
    | n == minBound =
        error $ "'pred' was called on (0 :: Unsigned " <> show (natToNatural @n)
             <> ") and caused an overflow. Use 'satPred' and specify a "
             <> "SaturationMode if you need other behavior."
    | otherwise = n -# fromInteger# 1

  toEnum         = toEnum#
  fromEnum       = fromEnum#
  enumFrom       = enumFrom#
  enumFromThen   = enumFromThen#
  enumFromTo     = enumFromTo#
  enumFromThenTo = enumFromThenTo#

toEnum# :: forall n. KnownNat n => Int -> Unsigned n
toEnum# = fromInteger# . toInteger
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE toEnum# #-}
{-# ANN toEnum# hasBlackBox #-}

fromEnum# :: forall n. KnownNat n => Unsigned n -> Int
fromEnum# = fromEnum . toInteger#
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE fromEnum# #-}
{-# ANN fromEnum# hasBlackBox #-}

enumFrom# :: forall n. KnownNat n => Unsigned n -> [Unsigned n]
enumFrom# = \x -> map (U . (`mod` m)) [unsafeToNatural x .. unsafeToNatural (maxBound :: Unsigned n)]
#if MIN_VERSION_base(4,15,0)
  where m = 1 `naturalShiftL` naturalToWord (natVal (Proxy @n))
#else
  where m = 1 `shiftL` fromInteger (natVal (Proxy @n))
#endif
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE enumFrom# #-}

enumFromThen# :: forall n. KnownNat n => Unsigned n -> Unsigned n -> [Unsigned n]
enumFromThen# = \x y -> toUnsigneds [unsafeToNatural x, unsafeToNatural y .. bound x y]
 where
  toUnsigneds = map (U . (`mod` m))
  bound x y = unsafeToNatural (if x <= y then maxBound else minBound :: Unsigned n)
#if MIN_VERSION_base(4,15,0)
  m = 1 `naturalShiftL` naturalToWord (natVal (Proxy @n))
#else
  m = 1 `shiftL` fromInteger (natVal (Proxy @n))
#endif
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE enumFromThen# #-}

enumFromTo# :: forall n. KnownNat n => Unsigned n -> Unsigned n -> [Unsigned n]
enumFromTo# = \x y -> map (U . (`mod` m)) [unsafeToNatural x .. unsafeToNatural y]
#if MIN_VERSION_base(4,15,0)
  where m = 1 `naturalShiftL` naturalToWord (natVal (Proxy @n))
#else
  where m = 1 `shiftL` fromInteger (natVal (Proxy @n))
#endif
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE enumFromTo# #-}

enumFromThenTo# :: forall n. KnownNat n => Unsigned n -> Unsigned n -> Unsigned n -> [Unsigned n]
enumFromThenTo# = \x1 x2 y -> map (U . (`mod` m)) [unsafeToNatural x1, unsafeToNatural x2 .. unsafeToNatural y]
#if MIN_VERSION_base(4,15,0)
  where m = 1 `naturalShiftL` naturalToWord (natVal (Proxy @n))
#else
  where m = 1 `shiftL` fromInteger (natVal (Proxy @n))
#endif
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE enumFromThenTo# #-}

instance KnownNat n => Bounded (Unsigned n) where
  minBound = minBound#
  maxBound = maxBound#

minBound# :: Unsigned n
minBound# = U 0
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE minBound# #-}
{-# ANN minBound# hasBlackBox #-}

maxBound# :: forall n. KnownNat n => Unsigned n
maxBound# = let m = 1 `shiftL` (natToNum @n) in  U (m - 1)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE maxBound# #-}
{-# ANN maxBound# hasBlackBox #-}

-- | __NB__: 'fromInteger'/'fromIntegral' can cause unexpected truncation, as
-- 'Integer' is arbitrarily bounded during synthesis.  Prefer
-- 'Clash.Class.BitPack.bitCoerce' and the 'Resize' class.
instance KnownNat n => Num (Unsigned n) where
  (+)         = (+#)
  (-)         = (-#)
  (*)         = (*#)
  negate      = negate#
  abs         = id
  signum bv   = resize# (unpack# (BV.pack# (reduceOr bv)))
  fromInteger = fromInteger#

(+#),(-#),(*#) :: forall n . KnownNat n => Unsigned n -> Unsigned n -> Unsigned n
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE (+#) #-}
{-# ANN (+#) hasBlackBox #-}
(+#) = \(U i) (U j) -> U (addMod m i j)
#if MIN_VERSION_base(4,15,0)
  where m = 1 `naturalShiftL` naturalToWord (natVal (Proxy @n))
#else
  where m = 1 `shiftL` fromInteger (natVal (Proxy @n))
#endif

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE (-#) #-}
{-# ANN (-#) hasBlackBox #-}
(-#) = \(U i) (U j) -> U (subMod m i j)
#if MIN_VERSION_base(4,15,0)
  where m = 1 `naturalShiftL` naturalToWord (natVal (Proxy @n))
#else
  where m = 1 `shiftL` fromInteger (natVal (Proxy @n))
#endif

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE (*#) #-}
{-# ANN (*#) hasBlackBox #-}
(*#) = \(U i) (U j) -> U (mulMod2 m i j)
#if MIN_VERSION_base(4,15,0)
  where m = (1 `naturalShiftL` naturalToWord (natVal (Proxy @n))) - 1
#else
  where m = (1 `shiftL` fromInteger (natVal (Proxy @n))) - 1
#endif

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE negate# #-}
{-# ANN negate# hasBlackBox #-}
negate# :: forall n . KnownNat n => Unsigned n -> Unsigned n
negate# = \(U i) -> U (negateMod m i)
#if MIN_VERSION_base(4,15,0)
  where m = 1 `naturalShiftL` naturalToWord (natVal (Proxy @n))
#else
  where m = 1 `shiftL` fromInteger (natVal (Proxy @n))
#endif

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE fromInteger# #-}
{-# ANN fromInteger# hasBlackBox #-}
fromInteger# :: forall n . KnownNat n => Integer -> Unsigned n
#if MIN_VERSION_base(4,15,0)
fromInteger# = \x -> U (integerToNatural (x `mod` m))
 where
  m = 1 `integerShiftL` naturalToWord (natVal (Proxy @n))
#else
fromInteger# = \x -> U (naturalFromInteger (x `mod` m))
 where
  m = 1 `shiftL` fromInteger (natVal (Proxy @n))
#endif

instance (KnownNat m, KnownNat n) => ExtendingNum (Unsigned m) (Unsigned n) where
  type AResult (Unsigned m) (Unsigned n) = Unsigned (Max m n + 1)
  add  = plus#
  sub = minus#
  type MResult (Unsigned m) (Unsigned n) = Unsigned (m + n)
  mul = times#

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE plus# #-}
{-# ANN plus# hasBlackBox #-}
plus# :: Unsigned m -> Unsigned n -> Unsigned (Max m n + 1)
plus# (U a) (U b) = U (a + b)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE minus# #-}
{-# ANN minus# hasBlackBox #-}
minus# :: forall m n . (KnownNat m, KnownNat n) => Unsigned m -> Unsigned n
                                                -> Unsigned (Max m n + 1)
minus# = \(U a) (U b) -> U (subMod mask a b)
 where
#if MIN_VERSION_base(4,15,0)
  sz   = naturalToWord (natVal (Proxy @(Max m n + 1)))
  mask = 1 `naturalShiftL` sz
#else
  sz   = fromInteger (natVal (Proxy @(Max m n + 1)))
  mask = 1 `shiftL` sz
#endif

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE times# #-}
{-# ANN times# hasBlackBox #-}
times# :: Unsigned m -> Unsigned n -> Unsigned (m + n)
times# (U a) (U b) = U (a * b)

instance KnownNat n => Real (Unsigned n) where
  toRational = toRational . toInteger#

-- | __NB__: 'toInteger'/'fromIntegral' can cause unexpected truncation, as
-- 'Integer' is arbitrarily bounded during synthesis.  Prefer
-- 'Clash.Class.BitPack.bitCoerce' and the 'Resize' class.
instance KnownNat n => Integral (Unsigned n) where
  quot        = quot#
  rem         = rem#
  div         = quot#
  mod         = rem#
  quotRem n d = (n `quot#` d,n `rem#` d)
  divMod  n d = (n `quot#` d,n `rem#` d)
  toInteger   = toInteger#

quot#,rem# :: Unsigned n -> Unsigned n -> Unsigned n
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE quot# #-}
{-# ANN quot# hasBlackBox #-}
quot# (U i) (U j) = U (i `quot` j)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE rem# #-}
{-# ANN rem# hasBlackBox #-}
rem# (U i) (U j) = U (i `rem` j)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE toInteger# #-}
{-# ANN toInteger# hasBlackBox #-}
toInteger# :: Unsigned n -> Integer
toInteger# (U i) = naturalToInteger i

instance KnownNat n => PrintfArg (Unsigned n) where
  formatArg = formatArg . toInteger

instance KnownNat n => Parity (Unsigned n) where
  even = even . pack
  odd = odd . pack

instance KnownNat n => Bits (Unsigned n) where
  (.&.)             = and#
  (.|.)             = or#
  xor               = xor#
  complement        = complement#
  zeroBits          = 0
  bit i             = replaceBit i high 0
  setBit v i        = replaceBit i high v
  clearBit v i      = replaceBit i low  v
  complementBit v i = replaceBit i (BV.complement## (v ! i)) v
  testBit v i       = v ! i == high
  bitSizeMaybe v    = Just (size# v)
  bitSize           = size#
  isSigned _        = False
  shiftL v i        = shiftL# v i
  shiftR v i        = shiftR# v i
  rotateL v i       = rotateL# v i
  rotateR v i       = rotateR# v i
  popCount u        = popCount (pack# u)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE and# #-}
{-# ANN and# hasBlackBox #-}
and# :: Unsigned n -> Unsigned n -> Unsigned n
and# (U v1) (U v2) = U (v1 .&. v2)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE or# #-}
{-# ANN or# hasBlackBox #-}
or# :: Unsigned n -> Unsigned n -> Unsigned n
or# (U v1) (U v2) = U (v1 .|. v2)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE xor# #-}
{-# ANN xor# hasBlackBox #-}
xor# :: Unsigned n -> Unsigned n -> Unsigned n
xor# (U v1) (U v2) = U (v1 `xor` v2)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE complement# #-}
{-# ANN complement# hasBlackBox #-}
complement# :: forall n . KnownNat n => Unsigned n -> Unsigned n
complement# = \(U i) -> U (complementN i)
  where complementN = complementMod (natVal (Proxy @n))

shiftL#, shiftR#, rotateL#, rotateR# :: forall n .KnownNat n => Unsigned n -> Int -> Unsigned n
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE shiftL# #-}
{-# ANN shiftL# hasBlackBox #-}
shiftL# = \(U v) i ->
#if MIN_VERSION_base(4,15,0)
  let i' = fromIntegral i in
  if | i < 0     -> error $ "'shiftL' undefined for negative number: " ++ show i
     | i' >= sz  -> U 0
     | otherwise -> U ((naturalShiftL v i') `mod` m)
 where
  sz = naturalToWord (natVal (Proxy @n))
  m  = 1 `naturalShiftL` sz
#else
  if | i < 0     -> error $ "'shiftL' undefined for negative number: " ++ show i
     | i >= sz   -> U 0
     | otherwise -> U ((shiftL v i) `mod` m)
 where
  sz = fromInteger (natVal (Proxy @n))
  m  = 1 `shiftL` sz
#endif

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE shiftR# #-}
{-# ANN shiftR# hasBlackBox #-}
-- shiftR# doesn't need the KnownNat constraint
-- But having the same type signature for all shift and rotate functions
-- makes implementing the Evaluator easier.
shiftR# (U v) i
  | i < 0     = error
              $ "'shiftR' undefined for negative number: " ++ show i
  | otherwise = U (shiftR v i)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE rotateL# #-}
{-# ANN rotateL# hasBlackBox #-}
rotateL# =
  \(U n) b ->
    if b >= 0 then
#if MIN_VERSION_base(4,15,0)
      let l   = naturalShiftL n b'
          r   = naturalShiftR n b''
          b'  = fromIntegral b `mod` sz
#else
      let l   = shiftL n b'
          r   = shiftR n b''
          b'  = b `mod` sz
#endif
          b'' = sz - b'
      in  U ((l .|. r) `mod` m)
    else
      error $ "'rotateL' undefined for negative number: " ++ show b
  where
#if MIN_VERSION_base(4,15,0)
    sz = naturalToWord (natVal (Proxy @n))
    m  = 1 `naturalShiftL` sz
#else
    sz = fromInteger (natVal (Proxy @n)) :: Int
    m  = 1 `shiftL` sz
#endif

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE rotateR# #-}
{-# ANN rotateR# hasBlackBox #-}
rotateR# =
  \(U n) b ->
    if b >= 0 then
#if MIN_VERSION_base(4,15,0)
      let l   = naturalShiftR n b'
          r   = naturalShiftL n b''
          b'  = fromIntegral b `mod` sz
#else
      let l   = shiftR n b'
          r   = shiftL n b''
          b'  = b `mod` sz
#endif
          b'' = sz - b'
      in  U ((l .|. r) `mod` m)
    else
      error $ "'rotateR' undefined for negative number: " ++ show b
  where
#if MIN_VERSION_base(4,15,0)
    sz = naturalToWord (natVal (Proxy @n))
    m  = 1 `naturalShiftL` sz
#else
    sz = fromInteger (natVal (Proxy @n)) :: Int
    m  = 1 `shiftL` sz
#endif


instance KnownNat n => FiniteBits (Unsigned n) where
  finiteBitSize        = size#
  countLeadingZeros  u = countLeadingZeros  (pack# u)
  countTrailingZeros u = countTrailingZeros (pack# u)

instance Resize Unsigned where
  resize     = resize#
  zeroExtend = extend
  truncateB  = resize#

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE resize# #-}
{-# ANN resize# hasBlackBox #-}
resize# :: forall n m . KnownNat m => Unsigned n -> Unsigned m
resize# = \(U i) -> if i >= m then U (i `mod` m) else U i
#if MIN_VERSION_base(4,15,0)
  where m = 1 `naturalShiftL` naturalToWord (natVal (Proxy @m))
#else
  where m = 1 `shiftL` fromInteger (natVal (Proxy @m))
#endif

instance Default (Unsigned n) where
  def = minBound#

instance KnownNat n => Lift (Unsigned n) where
  lift u@(U i) = sigE [| fromInteger# i |] (decUnsigned (natVal u))
  {-# NOINLINE lift #-}
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = liftTypedFromUntyped
#endif

#if MIN_VERSION_template_haskell(2,17,0)
decUnsigned :: Quote m => Natural -> m Type
decUnsigned n = appT (conT ''Unsigned) (litT $ numTyLit (integerFromNatural n))
#else
decUnsigned :: Integer -> TypeQ
decUnsigned n = appT (conT ''Unsigned) (litT $ numTyLit n)
#endif

instance KnownNat n => SaturatingNum (Unsigned n) where
  satAdd SatWrap a b = a +# b
  satAdd SatZero a b =
    let r = plus# a b
    in  case msb r of
          0 -> resize# r
          _ -> minBound#
  satAdd SatError a b =
    let r = plus# a b
    in  case msb r of
          0 -> resize# r
          _ -> errorX "Unsigned.satAdd: overflow"
  satAdd _ a b =
    let r  = plus# a b
    in  case msb r of
          0 -> resize# r
          _ -> maxBound#

  satSub SatWrap a b = a -# b
  satSub SatError a b =
    let r = minus# a b
    in  case msb r of
          0 -> resize# r
          _ -> errorX "Unsigned.satSub: overflow"
  satSub _ a b =
    let r = minus# a b
    in  case msb r of
          0 -> resize# r
          _ -> minBound#

  satMul SatWrap a b = a *# b
  satMul SatZero a b =
    let r       = times# a b
        (rL,rR) = split r
    in  case rL of
          0 -> unpack# rR
          _ -> minBound#
  satMul SatError a b =
    let r       = times# a b
        (rL,rR) = split r
    in  case rL of
          0 -> unpack# rR
          _ -> errorX "Unsigned.satMul: overflow"
  satMul _ a b =
    let r       = times# a b
        (rL,rR) = split r
    in  case rL of
          0 -> unpack# rR
          _ -> maxBound#

  -- Implementations for satSucc and satPred are needed because 1 :: Unsigned 0
  -- overflows to 0, meaning without the first check SatError would return 0.

  satSucc SatError a
    | a == maxBound = errorX "Unsigned.satSucc: overflow"
  satSucc satMode a = satAdd satMode a 1
  {-# INLINE satSucc #-}

  satPred SatError a
    | a == minBound = errorX "Unsigned.satPred: overflow"
  satPred satMode a = satSub satMode a 1
  {-# INLINE satPred #-}

instance KnownNat n => Arbitrary (Unsigned n) where
  arbitrary = arbitraryBoundedIntegral
  shrink    = BV.shrinkSizedUnsigned

instance KnownNat n => CoArbitrary (Unsigned n) where
  coarbitrary = coarbitraryIntegral

type instance Index   (Unsigned n) = Int
type instance IxValue (Unsigned n) = Bit
instance KnownNat n => Ixed (Unsigned n) where
  ix i f s = unpack# <$> BV.replaceBit# (pack# s) i
                     <$> f (BV.index# (pack# s) i)

instance (KnownNat n) => Ix (Unsigned n) where
  range (a, b) = [a..b]
  index ab@(a, b) x
    | inRange ab x = fromIntegral $ x - a
    | otherwise = error $ printf "Index (%d) out of range ((%d, %d))" x a b
  inRange (a, b) x = a <= x && x <= b

unsignedToWord :: Unsigned WORD_SIZE_IN_BITS -> Word
#if MIN_VERSION_base(4,15,0)
unsignedToWord (U (NS u#)) = W# u#
unsignedToWord (U (NB u#)) = bigNatToWord u#
#else
unsignedToWord (U (NatS# u#)) = W# u#
unsignedToWord (U (NatJ# u#)) = W# (bigNatToWord u#)
#endif
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unsignedToWord #-}
{-# ANN unsignedToWord hasBlackBox #-}

unsigned8toWord8 :: Unsigned 8 -> Word8
#if MIN_VERSION_base(4,16,0)
unsigned8toWord8 (U (NS u#)) = W8# (wordToWord8# u#)
unsigned8toWord8 (U (NB u#)) = W8# (wordToWord8# (bigNatToWord# u#))
#elif MIN_VERSION_base(4,15,0)
unsigned8toWord8 (U (NS u#)) = W8# (narrow8Word# u#)
unsigned8toWord8 (U (NB u#)) = W8# (narrow8Word# (bigNatToWord# u#))
#else
unsigned8toWord8 (U (NatS# u#)) = W8# (narrow8Word# u#)
unsigned8toWord8 (U (NatJ# u#)) = W8# (narrow8Word# (bigNatToWord u#))
#endif
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unsigned8toWord8 #-}
{-# ANN unsigned8toWord8 hasBlackBox #-}

unsigned16toWord16 :: Unsigned 16 -> Word16
#if MIN_VERSION_base(4,16,0)
unsigned16toWord16 (U (NS u#)) = W16# (wordToWord16# u#)
unsigned16toWord16 (U (NB u#)) = W16# (wordToWord16# (bigNatToWord# u#))
#elif MIN_VERSION_base(4,15,0)
unsigned16toWord16 (U (NS u#)) = W16# (narrow16Word# u#)
unsigned16toWord16 (U (NB u#)) = W16# (narrow16Word# (bigNatToWord# u#))
#else
unsigned16toWord16 (U (NatS# u#)) = W16# (narrow16Word# u#)
unsigned16toWord16 (U (NatJ# u#)) = W16# (narrow16Word# (bigNatToWord u#))
#endif
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unsigned16toWord16 #-}
{-# ANN unsigned16toWord16 hasBlackBox #-}

unsigned32toWord32 :: Unsigned 32 -> Word32
#if MIN_VERSION_base(4,16,0)
unsigned32toWord32 (U (NS u#)) = W32# (wordToWord32# u#)
unsigned32toWord32 (U (NB u#)) = W32# (wordToWord32# (bigNatToWord# u#))
#elif MIN_VERSION_base(4,15,0)
unsigned32toWord32 (U (NS u#)) = W32# (narrow32Word# u#)
unsigned32toWord32 (U (NB u#)) = W32# (narrow32Word# (bigNatToWord# u#))
#else
unsigned32toWord32 (U (NatS# u#)) = W32# (narrow32Word# u#)
unsigned32toWord32 (U (NatJ# u#)) = W32# (narrow32Word# (bigNatToWord u#))
#endif
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unsigned32toWord32 #-}
{-# ANN unsigned32toWord32 hasBlackBox #-}

{-# RULES
"bitCoerce/Unsigned WORD_SIZE_IN_BITS -> Word" bitCoerce = unsignedToWord
"bitCoerce/Unsigned 8 -> Word8" bitCoerce = unsigned8toWord8
"bitCoerce/Unsigned 16 -> Word16" bitCoerce = unsigned16toWord16
"bitCoerce/Unsigned 32 -> Word32" bitCoerce = unsigned32toWord32
 #-}
