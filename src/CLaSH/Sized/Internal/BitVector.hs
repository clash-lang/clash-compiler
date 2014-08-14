{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NegativeLiterals      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

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
    -- ** Concatenation
  , (#>)
  , (<#)
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
    -- ** Add
  , plus#
  , minus#
    -- ** Mult
  , mult#
    -- ** Integral
  , quot#
  , rem#
  , div#
  , mod#
  , quotRem#
  , divMod#
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

import Data.Default               (Default (..))
import Data.Bits                  (Bits (..))
import Data.Typeable              (Typeable)
import GHC.Integer                (smallInteger)
import GHC.Prim                   (dataToTag#)
import GHC.TypeLits               (KnownNat, Nat, type (+), type (-), natVal)
import Language.Haskell.TH        (TypeQ, appT, conT, litT, numTyLit, sigE)
import Language.Haskell.TH.Syntax (Lift(..))

import CLaSH.Class.Num            (Add (..), Mult (..), SaturatingNum (..),
                                   SaturationMode (..))
import CLaSH.Class.Resize         (Resize (..))
import CLaSH.Promoted.Nat         (SNat, snatToInteger)
import CLaSH.Promoted.Ord         (Max)

-- * Type definitions
newtype BitVector (n :: Nat) =
    -- | The constructor, 'BV', and  the field, 'unsafeToInteger', are not
    -- synthesisable.
    BV { unsafeToInteger :: Integer}
  deriving Typeable

type Bit = BitVector 1

-- * Instances
instance KnownNat n => Show (BitVector n) where
  show bv@(BV i) = showBV (natVal bv) i []
    where
      showBV 0 _ s = s
      showBV n v s = let (a,b) = divMod v 2
                     in  case b of
                           1 -> showBV (n - 1) a ('1':s)
                           _ -> showBV (n - 1) a ('0':s)

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

instance KnownNat (Max m n + 1) => Add (BitVector m) (BitVector n) where
  type AResult (BitVector m) (BitVector n) = BitVector (Max m n + 1)
  plus  = plus#
  minus = minus#

plus#, minus# :: KnownNat (Max m n + 1) => BitVector m -> BitVector n
              -> BitVector (Max m n + 1)
{-# NOINLINE plus# #-}
plus# (BV a) (BV b) = fromInteger_INLINE (a + b)

{-# NOINLINE minus# #-}
minus# (BV a) (BV b) = fromInteger_INLINE (a - b)

instance KnownNat (m + n) => Mult (BitVector m) (BitVector n) where
  type MResult (BitVector m) (BitVector n) = BitVector (m + n)
  mult = mult#

{-# NOINLINE mult# #-}
mult# :: KnownNat (m + n) => BitVector m -> BitVector n -> BitVector (m + n)
mult# (BV a) (BV b) = fromInteger_INLINE (a * b)

instance KnownNat n => Real (BitVector n) where
  toRational = toRational . toInteger#

instance KnownNat n => Integral (BitVector n) where
  quot      = quot#
  rem       = rem#
  div       = div#
  mod       = mod#
  quotRem   = quotRem#
  divMod    = divMod#
  toInteger = toInteger#

quot#,rem#,div#,mod# :: KnownNat n => BitVector n -> BitVector n -> BitVector n
{-# NOINLINE quot# #-}
quot# i j = case quotRem_INLINE i j of (a,_) -> a
{-# NOINLINE rem# #-}
rem# i j = case quotRem_INLINE i j of (_,b) -> b
{-# NOINLINE div# #-}
div# i j = case divMod_INLINE i j of (a,_) -> a
{-# NOINLINE mod# #-}
mod# i j = case divMod_INLINE i j of (_,b) -> b

quotRem#,divMod# :: KnownNat n => BitVector n -> BitVector n
                 -> (BitVector n, BitVector n)
quotRem# n d = (n `quot#` d,n `rem#` d)
divMod# n d  = (n `div#` d,n `mod#` d)

quotRem_INLINE,divMod_INLINE :: KnownNat n => BitVector n -> BitVector n
                             -> (BitVector n, BitVector n)
{-# INLINE quotRem_INLINE #-}
(BV a) `quotRem_INLINE` (BV b) = let (a',b') = a `quotRem` b
                                 in (BV a', BV b')
{-# INLINE divMod_INLINE #-}
(BV a) `divMod_INLINE` (BV b) = let (a',b') = a `divMod` b
                                in (BV a', BV b')

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
  testBit v i       = index# v i == 1
  bitSizeMaybe v    = Just (size# v)
  bitSize           = size#
  isSigned _        = False
  shiftL v i        = shiftL# v i
  shiftR v i        = shiftR# v i
  rotateL v i       = rotateL# v i
  rotateR v i       = rotateR# v i
  popCount          = popCount#

{-# NOINLINE reduceAnd# #-}
reduceAnd# :: Integer -> BitVector n -> BitVector 1
reduceAnd# sz (BV i) = BV (smallInteger (dataToTag# check))
  where
    check = i == maxI
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
{-# INLINABLE (#>) #-}
infixr 5 #>
-- | Prepend a 'Bit'
(#>) :: KnownNat n => Bit -> BitVector n -> BitVector (1 + n)
(#>) = (++#)

{-# INLINABLE (<#) #-}
infixl 5 <#
-- | Append a 'Bit'
(<#) :: BitVector n -> Bit -> BitVector (n + 1)
(<#) = (++#)

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

{-# NOINLINE and# #-}
and# :: BitVector n -> BitVector n -> BitVector n
and# (BV v1) (BV v2) = BV (v1 .&. v2)

{-# NOINLINE or# #-}
or# :: BitVector n -> BitVector n -> BitVector n
or# (BV v1) (BV v2) = BV (v1 .|. v2)

{-# NOINLINE xor# #-}
xor# :: BitVector n -> BitVector n -> BitVector n
xor# (BV v1) (BV v2) = BV (v1 `xor` v2)

{-# NOINLINE complement# #-}
complement# :: KnownNat n => BitVector n -> BitVector n
complement# (BV v1) = fromInteger_INLINE (complement v1)

{-# NOINLINE shiftL# #-}
shiftL#, shiftR#, rotateL#, rotateR# :: KnownNat n => BitVector n -> Int
                                     -> BitVector n
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

-- | A resize operation that zero-extends on extension.
--
-- Increasing the size of the BitVector extends with zeros to the left.
-- Truncating a BitVector of length N to a length L just removes the left
-- (most significant) N-L bits.
instance Resize BitVector where
  resize = resize#

{-# NOINLINE resize# #-}
resize# :: KnownNat m => BitVector n -> BitVector m
resize# (BV n) = fromInteger_INLINE n

instance KnownNat n => Lift (BitVector n) where
  lift bv@(BV i) = sigE [| fromInteger# i |] (decBitVector (natVal bv))

decBitVector :: Integer -> TypeQ
decBitVector n = appT (conT ''BitVector) (litT $ numTyLit n)

instance (KnownNat n, KnownNat (n + 1), KnownNat (n + n)) =>
  SaturatingNum (BitVector n) where
  satPlus = satPlus#
  satMin  = satMin#
  satMult = satMult#

satPlus#, satMin# :: (KnownNat n, KnownNat (n + 1)) => SaturationMode
                  -> BitVector n -> BitVector n -> BitVector n

satPlus# SatWrap a b = a +# b
satPlus# w a b = case msb# r of
                   0 -> resize# r
                   _ -> case w of
                          SatZero  -> minBound#
                          _        -> maxBound#
  where
    r = plus# a b

satMin# SatWrap a b = a -# b
satMin# _ a b = case msb# r of
                   0 -> resize# r
                   _ -> minBound#
  where
    r = minus# a b

satMult# :: (KnownNat n, KnownNat (n + n)) => SaturationMode -> BitVector n
         -> BitVector n -> BitVector n
satMult# SatWrap a b = a *# b
satMult# w a b = case rL of
                   0 -> rR
                   _ -> case w of
                          SatZero  -> minBound#
                          _        -> maxBound#
  where
    r       = mult# a b
    (rL,rR) = split# r
