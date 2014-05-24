{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module CLaSH.Sized.Unsigned
  ( Unsigned
  )
where

import Data.Bits
import Data.Default
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Syntax(Lift(..))
import GHC.TypeLits

import CLaSH.Bit
import CLaSH.Class.BitVector
import CLaSH.Class.Num
import CLaSH.Promoted.Ord
import CLaSH.Sized.Vector

-- | Arbitrary-width unsigned integer represented by @n@ bits
--
-- Given @n@ bits, an 'Unsigned' @n@ number has a range of: [0 .. 2^@n@-1]
--
-- NB: The 'Num' operators perform @wrap-around@ on overflow. If you want saturation
-- on overflow, check out the 'CLaSH.Sized.Fixed.satN2' function in "CLaSH.Sized.Fixed".
newtype Unsigned (n :: Nat) = U Integer
  deriving Typeable

instance Eq (Unsigned n) where
  (==) = eqU

{-# NOINLINE eqU #-}
eqU :: (Unsigned n) -> (Unsigned n) -> Bool
(U n) `eqU` (U m) = n == m

instance Ord (Unsigned n) where
  (<)  = ltU
  (>=) = geU
  (>)  = gtU
  (<=) = leU

ltU,geU,gtU,leU :: Unsigned n -> Unsigned n -> Bool
{-# NOINLINE ltU #-}
ltU (U n) (U m) = n < m
{-# NOINLINE geU #-}
geU (U n) (U m) = n >= m
{-# NOINLINE gtU #-}
gtU (U n) (U m) = n > m
{-# NOINLINE leU #-}
leU (U n) (U m) = n <= m

instance KnownNat n => Enum (Unsigned n) where
  succ           = plusU (fromIntegerU 1)
  pred           = minU (fromIntegerU 1)
  toEnum         = fromIntegerU . toInteger
  fromEnum       = fromEnum . toIntegerU
  enumFrom       = enumFromU
  enumFromThen   = enumFromThenU
  enumFromTo     = enumFromToU
  enumFromThenTo = enumFromThenToU

{-# NOINLINE enumFromU #-}
{-# NOINLINE enumFromThenU #-}
{-# NOINLINE enumFromToU #-}
{-# NOINLINE enumFromThenToU #-}
enumFromU       :: KnownNat n => Unsigned n -> [Unsigned n]
enumFromThenU   :: KnownNat n => Unsigned n -> Unsigned n -> [Unsigned n]
enumFromToU     :: KnownNat n => Unsigned n -> Unsigned n -> [Unsigned n]
enumFromThenToU :: KnownNat n => Unsigned n -> Unsigned n -> Unsigned n -> [Unsigned n]
enumFromU x             = map toEnum [fromEnum x ..]
enumFromThenU x y       = map toEnum [fromEnum x, fromEnum y ..]
enumFromToU x y         = map toEnum [fromEnum x .. fromEnum y]
enumFromThenToU x1 x2 y = map toEnum [fromEnum x1, fromEnum x2 .. fromEnum y]

instance KnownNat n => Bounded (Unsigned n) where
  minBound = fromIntegerU 0
  maxBound = maxBoundU

{-# NOINLINE maxBoundU #-}
maxBoundU :: KnownNat n => Unsigned n
maxBoundU = let res = U ((2 ^ natVal res) - 1) in res

-- | Operators do @wrap-around@ on overflow
instance KnownNat n => Num (Unsigned n) where
  (+)         = plusU
  (-)         = minU
  (*)         = timesU
  negate      = id
  abs         = id
  signum      = signumU
  fromInteger = fromIntegerU

plusU,minU,timesU :: KnownNat n => Unsigned n -> Unsigned n -> Unsigned n
{-# NOINLINE plusU #-}
plusU (U a) (U b) = fromIntegerU_inlineable $ a + b

{-# NOINLINE minU #-}
minU (U a) (U b) = fromIntegerU_inlineable $ a - b

{-# NOINLINE timesU #-}
timesU (U a) (U b) = fromIntegerU_inlineable $ a * b

{-# NOINLINE signumU #-}
signumU :: Unsigned n -> Unsigned n
signumU (U 0) = (U 0)
signumU (U _) = (U 1)

fromIntegerU,fromIntegerU_inlineable :: KnownNat n => Integer -> Unsigned n
{-# NOINLINE fromIntegerU #-}
fromIntegerU = fromIntegerU_inlineable
{-# INLINABLE fromIntegerU_inlineable #-}
fromIntegerU_inlineable i = let res = U (i `mod` (2 ^ natVal res)) in res

instance KnownNat (Max m n) => Add (Unsigned m) (Unsigned n) where
  type AResult (Unsigned m) (Unsigned n) = Unsigned (Max m n)
  plus  = plusU2
  minus = minusU2

plusU2, minusU2 :: KnownNat (Max m n) => Unsigned m -> Unsigned n -> Unsigned (Max m n)
{-# NOINLINE plusU2 #-}
plusU2 (U a) (U b) = fromIntegerU_inlineable (a + b)

{-# NOINLINE minusU2 #-}
minusU2 (U a) (U b) = fromIntegerU_inlineable (a - b)

instance KnownNat (m + n) => Mult (Unsigned m) (Unsigned n) where
  type MResult (Unsigned m) (Unsigned n) = Unsigned (m + n)
  mult = multU2

{-# NOINLINE multU2 #-}
multU2 :: KnownNat (m + n) => Unsigned m -> Unsigned n -> Unsigned (m + n)
multU2 (U a) (U b) = fromIntegerU_inlineable (a * b)

instance KnownNat n => Real (Unsigned n) where
  toRational = toRational . toIntegerU

instance KnownNat n => Integral (Unsigned n) where
  quot      = quotU
  rem       = remU
  div       = quotU
  mod       = modU
  quotRem   = quotRemU
  divMod    = divModU
  toInteger = toIntegerU

quotU,remU,modU :: KnownNat n => Unsigned n -> Unsigned n -> Unsigned n
{-# NOINLINE quotU #-}
quotU = (fst.) . quotRemU_inlineable
{-# NOINLINE remU #-}
remU = (snd.) . quotRemU_inlineable
{-# NOINLINE modU #-}
(U a) `modU` (U b) = fromIntegerU_inlineable (a `mod` b)

quotRemU,divModU :: KnownNat n => Unsigned n -> Unsigned n -> (Unsigned n, Unsigned n)
quotRemU n d = (n `quotU` d,n `remU` d)
divModU n d  = (n `quotU` d,n `modU` d)

{-# INLINEABLE quotRemU_inlineable #-}
quotRemU_inlineable :: KnownNat n => Unsigned n -> Unsigned n -> (Unsigned n, Unsigned n)
(U a) `quotRemU_inlineable` (U b) = let (a',b') = a `quotRem` b
                                    in (fromIntegerU_inlineable a', fromIntegerU_inlineable b')

{-# NOINLINE toIntegerU #-}
toIntegerU :: Unsigned n -> Integer
toIntegerU (U n) = n

instance KnownNat n => Bits (Unsigned n) where
  (.&.)          = andU
  (.|.)          = orU
  xor            = xorU
  complement     = complementU
  bit            = bitU
  testBit        = testBitU
  bitSizeMaybe   = Just . finiteBitSizeU
  isSigned       = const False
  shiftL         = shiftLU
  shiftR         = shiftRU
  rotateL        = rotateLU
  rotateR        = rotateRU
  popCount       = popCountU

andU,orU,xorU :: KnownNat n => Unsigned n -> Unsigned n -> Unsigned n
{-# NOINLINE andU #-}
(U a) `andU` (U b) = fromIntegerU_inlineable (a .&. b)
{-# NOINLINE orU #-}
(U a) `orU` (U b)  = fromIntegerU_inlineable (a .|. b)
{-# NOINLINE xorU #-}
(U a) `xorU` (U b) = fromIntegerU_inlineable (xor a b)

{-# NOINLINE complementU #-}
complementU :: KnownNat n => Unsigned n -> Unsigned n
complementU = fromBitVector . vmap complement . toBitVector

{-# NOINLINE bitU #-}
bitU :: KnownNat n => Int -> Unsigned n
bitU i = res
  where
    sz = finiteBitSizeU res
    res | sz > i    = fromIntegerU_inlineable (bit i)
        | otherwise = error $ concat [ "bit: "
                                     , "Setting out-of-range bit position, size: "
                                     , show sz
                                     , ", position: "
                                     , show i
                                     ]

{-# NOINLINE testBitU #-}
testBitU :: KnownNat n => Unsigned n -> Int -> Bool
testBitU s@(U n) i
  | sz > i    = testBit n i
  | otherwise = error $ concat [ "testBit: "
                               , "Setting out-of-range bit position, size: "
                               , show sz
                               , ", position: "
                               , show i
                               ]
  where
    sz = finiteBitSizeU s

shiftLU,shiftRU,rotateLU,rotateRU :: KnownNat n => Unsigned n -> Int -> Unsigned n
{-# NOINLINE shiftLU #-}
shiftLU _ b | b < 0  = error "'shiftL'{Unsigned} undefined for negative numbers"
shiftLU (U n) b      = fromIntegerU_inlineable (shiftL n b)
{-# NOINLINE shiftRU #-}
shiftRU _ b | b < 0  = error "'shiftR'{Unsigned} undefined for negative numbers"
shiftRU (U n) b      = fromIntegerU_inlineable (shiftR n b)
{-# NOINLINE rotateLU #-}
rotateLU _ b | b < 0 = error "'shiftL'{Unsigned} undefined for negative numbers"
rotateLU n b         = let b' = b `mod` finiteBitSizeU n
                       in shiftL n b' .|. shiftR n (finiteBitSizeU n - b')
{-# NOINLINE rotateRU #-}
rotateRU _ b | b < 0 = error "'shiftR'{Unsigned} undefined for negative numbers"
rotateRU n b         = let b' = b `mod` finiteBitSizeU n
                       in shiftR n b' .|. shiftL n (finiteBitSizeU n - b')

{-# NOINLINE popCountU #-}
popCountU :: Unsigned n -> Int
popCountU (U n) = popCount n

instance KnownNat n => FiniteBits (Unsigned n) where
  finiteBitSize  = finiteBitSizeU

{-# NOINLINE finiteBitSizeU #-}
finiteBitSizeU :: KnownNat n => Unsigned n -> Int
finiteBitSizeU u = fromInteger (natVal u)

instance KnownNat n => Lift (Unsigned n) where
  lift u@(U i) = sigE [| fromIntegerU i |] (decUnsigned (natVal u))

decUnsigned :: Integer -> TypeQ
decUnsigned n = appT (conT ''Unsigned) (litT $ numTyLit n)

instance Show (Unsigned n) where
  show (U n) = show n

instance KnownNat n => Default (Unsigned n) where
  def = fromIntegerU 0

instance BitVector (Unsigned n) where
  type BitSize (Unsigned n) = n
  toBV   = toBitVector
  fromBV = fromBitVector

{-# NOINLINE toBitVector #-}
toBitVector :: KnownNat n => Unsigned n -> Vec n Bit
toBitVector (U m) = vreverse $ vmap (\x -> if odd x then H else L) $ viterateI (`div` 2) m

{-# NOINLINE fromBitVector #-}
fromBitVector :: KnownNat n => Vec n Bit -> Unsigned n
fromBitVector = fromBitList . reverse . toList

{-# INLINABLE fromBitList #-}
fromBitList :: KnownNat n => [Bit] -> Unsigned n
fromBitList l = fromIntegerU_inlineable
              $ sum [ n
                    | (n,b) <- zip (iterate (*2) 1) l
                    , b == H
                    ]

{-# NOINLINE resizeU #-}
resizeU :: KnownNat m => Unsigned n -> Unsigned m
resizeU (U n) = fromIntegerU_inlineable n

-- | A resize operation that zero-extends on extension, and wraps on truncation.
--
-- Increasing the size of the number extends with zeros to the left.
-- Truncating a number of length N to a length L just removes the left
-- (most significant) N-L bits.
instance Resize Unsigned where
  resize = resizeU
