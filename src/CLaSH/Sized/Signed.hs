{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module CLaSH.Sized.Signed
  ( Signed
  , resizeS
  , resizeS_wrap
  )
where

import Data.Bits
import Data.Default
import Language.Haskell.TH
import Language.Haskell.TH.Syntax(Lift(..))
import GHC.TypeLits

import CLaSH.Bit
import CLaSH.Class.BitVector
import CLaSH.Class.Num
import CLaSH.Promoted.Ord
import CLaSH.Sized.Vector

-- | Arbitrary precision signed integer represented by @n@ bits
newtype Signed (n :: Nat) = S Integer

instance Eq (Signed n) where
  (==) = eqS

{-# NOINLINE eqS #-}
eqS :: (Signed n) -> (Signed n) -> Bool
(S n) `eqS` (S m) = n == m

instance Ord (Signed n) where
  (<)  = ltS
  (>=) = geS
  (>)  = gtS
  (<=) = leS

ltS,geS,gtS,leS :: Signed n -> Signed n -> Bool
{-# NOINLINE ltS #-}
ltS (S n) (S m) = n < m
{-# NOINLINE geS #-}
geS (S n) (S m) = n >= m
{-# NOINLINE gtS #-}
gtS (S n) (S m) = n > m
{-# NOINLINE leS #-}
leS (S n) (S m) = n <= m

instance KnownNat n => Enum (Signed n) where
  succ           = plusS (fromIntegerS 1)
  pred           = minS (fromIntegerS 1)
  toEnum         = fromIntegerS . toInteger
  fromEnum       = fromEnum . toIntegerS

instance KnownNat n => Bounded (Signed n) where
  minBound = minBoundS
  maxBound = maxBoundS

minBoundS,maxBoundS :: KnownNat n => Signed n
{-# NOINLINE minBoundS #-}
minBoundS = let res = S $ negate $ 2 ^ (natVal res - 1) in res
{-# NOINLINE maxBoundS #-}
maxBoundS = let res = S $ 2 ^ (natVal res - 1) - 1 in res

instance KnownNat n => Num (Signed n) where
  (+)         = plusS
  (-)         = minS
  (*)         = timesS
  negate      = negateS
  abs         = absS
  signum      = signumS
  fromInteger = fromIntegerS

plusS,minS,timesS :: KnownNat n => Signed n -> Signed n -> Signed n
{-# NOINLINE plusS #-}
plusS (S a) (S b) = fromIntegerS_inlineable (a + b)

{-# NOINLINE minS #-}
minS (S a) (S b) = fromIntegerS_inlineable (a - b)

{-# NOINLINE timesS #-}
timesS (S a) (S b) = fromIntegerS_inlineable (a * b)

negateS,absS,signumS :: KnownNat n => Signed n -> Signed n
{-# NOINLINE negateS #-}
negateS (S n) = fromIntegerS_inlineable (0 - n)

{-# NOINLINE absS #-}
absS (S n) = fromIntegerS_inlineable (abs n)

{-# NOINLINE signumS #-}
signumS (S n) = fromIntegerS_inlineable (signum n)

fromIntegerS,fromIntegerS_inlineable :: KnownNat n => Integer -> Signed (n :: Nat)
{-# NOINLINE fromIntegerS #-}
fromIntegerS = fromIntegerS_inlineable
{-# INLINABLE fromIntegerS_inlineable #-}
fromIntegerS_inlineable i
    | nS == 0   = S 0
    | otherwise = res
  where
    nS  = natVal res
    sz  = 2 ^ (nS - 1)
    res = case divMod i sz of
            (s,i') | even s    -> S i'
                   | otherwise -> S (i' - sz)

instance KnownNat (Max m n) => Add (Signed m) (Signed n) where
  type AResult (Signed m) (Signed n) = Signed (Max m n)
  plus  = plusS2
  minus = minusS2

plusS2, minusS2 :: KnownNat (Max m n) => Signed m -> Signed n -> Signed (Max m n)
{-# NOINLINE plusS2 #-}
plusS2 (S a) (S b) = fromIntegerS_inlineable (a + b)

{-# NOINLINE minusS2 #-}
minusS2 (S a) (S b) = fromIntegerS_inlineable (a - b)

instance KnownNat (m + n) => Mult (Signed m) (Signed n) where
  type MResult (Signed m) (Signed n) = Signed (m + n)
  mult = multS2

{-# NOINLINE multS2 #-}
multS2 :: KnownNat (m + n) => Signed m -> Signed n -> Signed (m + n)
multS2 (S a) (S b) = fromIntegerS_inlineable (a * b)

instance KnownNat n => Real (Signed n) where
  toRational = toRational . toIntegerS

instance KnownNat n => Integral (Signed n) where
  quot      = quotS
  rem       = remS
  div       = divS
  mod       = modS
  quotRem   = quotRemS
  divMod    = divModS
  toInteger = toIntegerS

quotS,remS,divS,modS :: KnownNat n => Signed n -> Signed n -> Signed n
{-# NOINLINE quotS #-}
quotS = (fst.) . quotRemS_inlineable
{-# NOINLINE remS #-}
remS = (snd.) . quotRemS_inlineable
{-# NOINLINE divS #-}
divS = (fst.) . divModS_inlineable
{-# NOINLINE modS #-}
modS = (snd.) . divModS_inlineable

quotRemS,divModS :: KnownNat n => Signed n -> Signed n -> (Signed n, Signed n)
quotRemS n d = (n `quotS` d,n `remS` d)
divModS n d  = (n `divS` d,n `modS` d)

quotRemS_inlineable,divModS_inlineable :: KnownNat n => Signed n -> Signed n -> (Signed n, Signed n)
{-# INLINEABLE quotRemS_inlineable #-}
(S a) `quotRemS_inlineable` (S b) = let (a',b') = a `quotRem` b
                                    in (fromIntegerS_inlineable a', fromIntegerS_inlineable b')
{-# INLINEABLE divModS_inlineable #-}
(S a) `divModS_inlineable` (S b) = let (a',b') = a `divMod` b
                                   in (fromIntegerS_inlineable a', fromIntegerS_inlineable b')

{-# NOINLINE toIntegerS #-}
toIntegerS :: Signed n -> Integer
toIntegerS (S n) = n

instance KnownNat n => Bits (Signed n) where
  (.&.)          = andS
  (.|.)          = orS
  xor            = xorS
  complement     = complementS
  bit            = bitS
  testBit        = testBitS
  bitSizeMaybe   = Just . finiteBitSizeS
  isSigned       = const True
  shiftL         = shiftLS
  shiftR         = shiftRS
  rotateL        = rotateLS
  rotateR        = rotateRS
  popCount       = popCountS

andS,orS,xorS :: KnownNat n => Signed n -> Signed n -> Signed n
{-# NOINLINE andS #-}
(S a) `andS` (S b) = fromIntegerS_inlineable (a .&. b)
{-# NOINLINE orS #-}
(S a) `orS` (S b)  = fromIntegerS_inlineable (a .|. b)
{-# NOINLINE xorS #-}
(S a) `xorS` (S b) = fromIntegerS_inlineable (xor a b)

{-# NOINLINE complementS #-}
complementS :: KnownNat n => Signed n -> Signed n
complementS = fromBitVector . vmap complement . toBitVector

{-# NOINLINE bitS #-}
bitS :: KnownNat n => Int -> Signed n
bitS i = res
  where
    sz = finiteBitSizeS res
    res | sz > i    = fromIntegerS_inlineable (bit i)
        | otherwise = error $ concat [ "bit: "
                                     , "Setting out-of-range bit position, size: "
                                     , show sz
                                     , ", position: "
                                     , show i
                                     ]

{-# NOINLINE testBitS #-}
testBitS :: KnownNat n => Signed n -> Int -> Bool
testBitS s@(S n) i
  | sz > i    = testBit n i
  | otherwise = error $ concat [ "testBit: "
                               , "Setting out-of-range bit position, size: "
                               , show sz
                               , ", position: "
                               , show i
                               ]
  where
    sz = finiteBitSizeS s

shiftLS,shiftRS,rotateLS,rotateRS :: KnownNat n => Signed n -> Int -> Signed n
{-# NOINLINE shiftLS #-}
shiftLS _ b | b < 0  = error "'shiftL'{Signed} undefined for negative numbers"
shiftLS (S n) b      = fromIntegerS_inlineable (shiftL n b)
{-# NOINLINE shiftRS #-}
shiftRS _ b | b < 0  = error "'shiftR'{Signed} undefined for negative numbers"
shiftRS (S n) b      = fromIntegerS_inlineable (shiftR n b)
{-# NOINLINE rotateLS #-}
rotateLS _ b | b < 0 = error "'shiftL'{Signed} undefined for negative numbers"
rotateLS n b         = let b' = b `mod` finiteBitSizeS n
                       in shiftL n b' .|. shiftR n (finiteBitSizeS n - b')
{-# NOINLINE rotateRS #-}
rotateRS _ b | b < 0 = error "'shiftR'{Signed} undefined for negative numbers"
rotateRS n b         = let b' = b `mod` finiteBitSizeS n
                       in shiftR n b' .|. shiftL n (finiteBitSizeS n - b')

{-# NOINLINE popCountS #-}
popCountS :: Signed n -> Int
popCountS (S n) = popCount n

instance KnownNat n => FiniteBits (Signed n) where
  finiteBitSize = finiteBitSizeS

{-# NOINLINE finiteBitSizeS #-}
finiteBitSizeS :: KnownNat n => Signed n -> Int
finiteBitSizeS i = let res = fromInteger (natVal i) in res

instance Show (Signed n) where
  show (S n) = show n

instance KnownNat n => Default (Signed n) where
  def = fromIntegerS 0

instance KnownNat n => Lift (Signed n) where
  lift s@(S i) = sigE [| fromIntegerS i |] (decSigned (natVal s))

decSigned :: Integer -> TypeQ
decSigned n = appT (conT ''Signed) (litT $ numTyLit n)

instance BitVector (Signed n) where
  type BitSize (Signed n) = n
  toBV   = toBitVector
  fromBV = fromBitVector

{-# NOINLINE toBitVector #-}
toBitVector :: KnownNat n => Signed n -> Vec n Bit
toBitVector (S m) = vreverse $ vmap (\x -> if odd x then H else L) $ viterateI (`div` 2) m

{-# NOINLINE fromBitVector #-}
fromBitVector :: KnownNat n => Vec n Bit -> Signed n
fromBitVector = fromBitList . reverse . toList

{-# INLINABLE fromBitList #-}
fromBitList :: KnownNat n => [Bit] -> Signed n
fromBitList l = fromIntegerS_inlineable
              $ sum [ n
                    | (n,b) <- zip (iterate (*2) 1) l
                    , b == H
                    ]

{-# NOINLINE resizeS #-}
-- | A sign-preserving resize operation
--
-- Increasing the size of the number replicates the sign bit to the left.
-- Truncating a number to length L keeps the sign bit and the rightmost L-1 bits.
--
resizeS :: (KnownNat n, KnownNat m) => Signed n -> Signed m
resizeS s@(S n) | n' <= m'  = extend
                | otherwise = trunc
  where
    n'     = fromInteger (natVal s)
    m'     = fromInteger (natVal extend)
    extend = fromIntegerS_inlineable n
    trunc  = case toList (toBitVector s) of
                    (x:xs) -> fromBitList $ reverse $ x : (drop (n' - m') xs)
                    _      -> error "resizeS impossible case: empty list"

{-# NOINLINE resizeS_wrap #-}
-- | A resize operation that is sign-preserving on extension, but wraps on truncation.
--
-- Increasing the size of the number replicates the sign bit to the left.
-- Truncating a number of length N to a length L just removes the leftmost N-L bits.
--
resizeS_wrap :: KnownNat m => Signed n -> Signed m
resizeS_wrap (S n) = fromIntegerS_inlineable n
