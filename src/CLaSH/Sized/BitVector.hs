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

module CLaSH.Sized.BitVector
  ( -- * Datatypes
    BitVector
  , Bit
    -- * Accessors
    -- ** Length information
  , size#
  , maxIndex#
    -- ** Indexing
  , (!#)
  , msb#
  , lsb#
    -- * Construction
    -- ** Initialisation
  , high
  , low
    -- ** Concatenation
  , (#>)
  , (<#)
  , (++#)
    -- * Modifying BitVectors
  , setBit#
  , split
    -- * Bitwise operations
  , and#
  , or#
  , xor#
  , complement#
  , shiftL#
  , shiftR#
  , rotateL#
  , rotateR#
  -- * __VERY__ unsafe operations, that are not synthesizable
  , veryUnsafeToInteger#
  , veryUnsafeFromInteger#
  )
where

import Data.Default               (Default (..))
import Data.Bits                  ((.&.), (.|.), complement, popCount, setBit,
                                   shiftL, shiftR, testBit, xor)
import Data.Typeable              (Typeable)
import GHC.Integer                (smallInteger)
import GHC.Prim                   (dataToTag#)
import GHC.TypeLits               (KnownNat, Nat, type (+), natVal)
import Language.Haskell.TH        (TypeQ, appT, conT, litT, numTyLit, sigE)
import Language.Haskell.TH.Syntax (Lift(..))

import CLaSH.Class.BitReduction   (BitReduction (..))
import CLaSH.Class.Num            (Add (..), Mult (..))
import CLaSH.Class.Resize         (Resize (..))
import CLaSH.Promoted.Ord         (Max)

-- * Type definitions
newtype BitVector (n :: Nat) = BV { veryUnsafeToInteger# :: Integer}
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
enumFromThenTo# :: KnownNat n => BitVector n -> BitVector n -> BitVector n -> [BitVector n]
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

instance BitReduction BitVector where
  reduceAnd  = reduceAnd#
  reduceOr   = reduceOr#
  reduceXor  = reduceXor#

reduceAnd#, reduceOr#, reduceXor# :: KnownNat n => BitVector n -> BitVector 1

{-# NOINLINE reduceAnd# #-}
reduceAnd# bv@(BV i) = BV (smallInteger (dataToTag# check))
  where
    check = i == maxI
    maxI  = (2 ^ natVal bv) - 1

{-# NOINLINE reduceOr# #-}
reduceOr# (BV i) = BV (smallInteger (dataToTag# check))
  where
    check = i /= 0

{-# NOINLINE reduceXor# #-}
reduceXor# (BV i) = BV (toInteger (popCount i `mod` 2))

instance Default (BitVector n) where
  def = minBound#

-- * Accessors
-- ** Length information
{-# NOINLINE size# #-}
size# :: KnownNat n => BitVector n -> Integer
size# bv = natVal bv

{-# NOINLINE maxIndex# #-}
maxIndex# :: KnownNat n => BitVector n -> Integer
maxIndex# bv = natVal bv - 1

-- ** Indexing
{-# NOINLINE (!#) #-}
(!#) :: KnownNat n => BitVector n -> Int -> Bit
bv@(BV v) !# i
    | i >= 0 && i <= maxI = BV (smallInteger (dataToTag# (testBit v i)))
    | otherwise           = err
  where
    maxI = fromInteger (natVal bv) - 1
    err  = error $ concat [ "!#: "
                          , show i
                          , " is out of range ["
                          , show maxI
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
lsb# :: KnownNat n => BitVector n -> Bit
lsb# (BV v) = BV (smallInteger (dataToTag# (testBit v 0)))

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

{-# INLINE veryUnsafeFromInteger# #-}
veryUnsafeFromInteger# :: Integer -> BitVector n
veryUnsafeFromInteger# = BV

-- ** Concatenation
{-# INLINABLE (#>) #-}
infixr 5 #>
-- | Prepend a 'Bit'
(#>) :: Bit -> BitVector n -> BitVector (1 + n)
(#>) = (++#)

{-# INLINABLE (<#) #-}
infixl 5 <#
-- | Append a 'Bit'
(<#) :: KnownNat n => BitVector n -> Bit -> BitVector (n + 1)
(<#) = (++#)

{-# NOINLINE (++#) #-}
-- | Concatenate two 'BitVector's
(++#) :: KnownNat n => BitVector n -> BitVector m -> BitVector (n + m)
bv1@(BV v1) ++# (BV v2) = BV (v1' + v2)
  where
    v1' = shiftL v1 (fromInteger (natVal bv1))

-- * Modifying BitVectors
{-# NOINLINE setBit# #-}
setBit# :: KnownNat n => BitVector n -> Int -> BitVector n
setBit# bv@(BV v) i
    | i >= 0 && i <= maxI = BV (setBit v i)
    | otherwise           = err
  where
    maxI = fromInteger (natVal bv) - 1
    err  = error $ concat [ "setBit#: "
                          , show i
                          , " is out of range ["
                          , show maxI
                          , "..0]"
                          ]

{-# NOINLINE split #-}
split :: KnownNat n => BitVector (m + n) -> (BitVector m, BitVector n)
split (BV i) = (l,r)
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
complement# :: BitVector n -> BitVector n
complement# (BV v1) = BV (complement v1)

{-# NOINLINE shiftL# #-}
shiftL#, shiftR#, rotateL#, rotateR# :: KnownNat n => BitVector n -> Int
                                     -> BitVector n
shiftL# (BV v) i
  | i < 0     = error
              $ "'shiftL undefined for negative number: " ++ show i
  | otherwise = fromInteger# (shiftL v i)

{-# NOINLINE shiftR# #-}
shiftR# (BV v) i
  | i < 0     = error
              $ "'shiftR undefined for negative number: " ++ show i
  | otherwise = fromInteger# (shiftR v i)

{-# NOINLINE rotateL# #-}
rotateL# _ b | b < 0 = error "'shiftL undefined for negative numbers"
rotateL# bv@(BV n) b   = fromInteger_INLINE (l .|. r)
  where
    l    = shiftL n b'
    r    = shiftR n b'' .&. mask
    mask = 2 ^ b' - 1

    b'   = b `mod` sz
    b''  = sz - b'
    sz   = fromInteger (natVal bv)

{-# NOINLINE rotateR# #-}
rotateR# _ b | b < 0 = error "'shiftR undefined for negative numbers"
rotateR# bv@(BV n) b   = fromInteger_INLINE (l .|. r)
  where
    l    = shiftR n b' .&. mask
    r    = shiftL n b''
    mask = 2 ^ b'' - 1

    b'  = b `mod` sz
    b'' = sz - b'
    sz  = fromInteger (natVal bv)

-- toBitList :: KnownNat n => BitVector n -> [Bit]
-- toBitList bv@(BV i) = toBitList' (natVal bv) i []
--   where
--     toBitList' 0 _ l = l
--     toBitList' n v l = let (a,b) = divMod v 2
--                        in  toBitList' (n - 1) a ((BV b):l)

-- fromBitList :: KnownNat n => [Bit] -> BitVector n
-- fromBitList l = bv
--   where
--     bv   = BV (sum [n | (n,b) <- zip pows l, b == (BV 1)])
--     sz   = natVal bv
--     pows = iterate (`div` 2) (2 ^ (sz - 1))

-- | A resize operation that zero-extends on extension, and wraps on truncation.
--
-- Increasing the size of the number extends with zeros to the left.
-- Truncating a number of length N to a length L just removes the left
-- (most significant) N-L bits.
instance Resize BitVector where
  resize = resize#

{-# NOINLINE resize# #-}
resize# :: KnownNat m => BitVector n -> BitVector m
resize# (BV n) = fromInteger_INLINE n

instance KnownNat n => Lift (BitVector n) where
  lift bv@(BV i) = sigE [| fromInteger# i |] (decUnsigned (natVal bv))

decUnsigned :: Integer -> TypeQ
decUnsigned n = appT (conT ''BitVector) (litT $ numTyLit n)
