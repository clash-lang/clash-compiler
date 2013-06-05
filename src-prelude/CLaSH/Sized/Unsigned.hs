{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module CLaSH.Sized.Unsigned
  ( Unsigned
  , resizeU
  )
where

import Data.Bits
import Language.Haskell.TH
import Language.Haskell.TH.Syntax(Lift(..))
import GHC.TypeLits

import CLaSH.Bit
import CLaSH.Class.BitVector
import CLaSH.Class.Default
import CLaSH.Sized.VectorZ

newtype Unsigned (n :: Nat) = U Integer

instance Eq (Unsigned n) where
  (==) = eqU

{-# NOINLINE eqU #-}
eqU :: (Unsigned n) -> (Unsigned n) -> Bool
(U n) `eqU` (U m) = n == m

instance Ord (Unsigned n) where
  compare n m =
    if geU n m
      then if gtU n m then GT else EQ
      else LT
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

instance SingI n => Enum (Unsigned n) where
  succ           = plusU (fromIntegerU 1)
  pred           = minU (fromIntegerU 1)
  toEnum         = fromIntegerU . toInteger
  fromEnum       = fromEnum . toIntegerU

instance SingI n => Bounded (Unsigned n) where
  minBound = fromIntegerU 0
  maxBound = maxBoundU

{-# NOINLINE maxBoundU #-}
maxBoundU :: forall n . SingI n => Unsigned n
maxBoundU = U $ (2 ^ fromSing (sing :: Sing n)) - 1

instance SingI n => Num (Unsigned n) where
  (+)         = plusU
  (-)         = minU
  (*)         = timesU
  negate      = id
  abs         = id
  signum      = signumU
  fromInteger = fromIntegerU

plusU,minU,timesU :: SingI n => Unsigned n -> Unsigned n -> Unsigned n
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

fromIntegerU,fromIntegerU_inlineable :: forall n . SingI n => Integer -> Unsigned (n :: Nat)
{-# NOINLINE fromIntegerU #-}
fromIntegerU = fromIntegerU_inlineable
{-# INLINABLE fromIntegerU_inlineable #-}
fromIntegerU_inlineable i = U $ i `mod` (2 ^ fromSing (sing :: Sing n))

instance SingI n => Real (Unsigned n) where
  toRational = toRational . toIntegerU

instance SingI n => Integral (Unsigned n) where
  quot      = quotU
  rem       = remU
  div       = quotU
  mod       = modU
  quotRem   = quotRemU
  divMod    = divModU
  toInteger = toIntegerU

quotU,remU,modU :: SingI n => Unsigned n -> Unsigned n -> Unsigned n
{-# NOINLINE quotU #-}
quotU = (fst.) . quotRemU_inlineable
{-# NOINLINE remU #-}
remU = (snd.) . quotRemU_inlineable
{-# NOINLINE modU #-}
(U a) `modU` (U b) = fromIntegerU_inlineable (a `mod` b)

quotRemU,divModU :: SingI n => Unsigned n -> Unsigned n -> (Unsigned n, Unsigned n)
quotRemU n d = (n `quotU` d,n `remU` d)
divModU n d  = (n `quotU` d,n `modU` d)

{-# INLINEABLE quotRemU_inlineable #-}
quotRemU_inlineable :: SingI n => Unsigned n -> Unsigned n -> (Unsigned n, Unsigned n)
(U a) `quotRemU_inlineable` (U b) = let (a',b') = a `quotRem` b
                                    in (fromIntegerU_inlineable a', fromIntegerU_inlineable b')

{-# NOINLINE toIntegerU #-}
toIntegerU :: Unsigned n -> Integer
toIntegerU (U n) = n

instance SingI n => Bits (Unsigned n) where
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

andU,orU,xorU :: SingI n => Unsigned n -> Unsigned n -> Unsigned n
{-# NOINLINE andU #-}
(U a) `andU` (U b) = fromIntegerU_inlineable (a .&. b)
{-# NOINLINE orU #-}
(U a) `orU` (U b)  = fromIntegerU_inlineable (a .|. b)
{-# NOINLINE xorU #-}
(U a) `xorU` (U b) = fromIntegerU_inlineable (xor a b)

{-# NOINLINE complementU #-}
complementU :: SingI n => Unsigned n -> Unsigned n
complementU = fromBitVector . vmap complement . toBitVector

{-# NOINLINE bitU #-}
bitU :: SingI n => Int -> Unsigned n
bitU = fromIntegerU_inlineable . bit

{-# NOINLINE testBitU #-}
testBitU :: Unsigned n -> Int -> Bool
testBitU (U n) i = testBit n i

shiftLU,shiftRU,rotateLU,rotateRU :: SingI n => Unsigned n -> Int -> Unsigned n
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

instance SingI n => FiniteBits (Unsigned n) where
  finiteBitSize  = finiteBitSizeU

{-# NOINLINE finiteBitSizeU #-}
finiteBitSizeU :: forall n . SingI n => Unsigned n -> Int
finiteBitSizeU _ = fromInteger $ fromSing (sing :: Sing n)

instance forall n . SingI n => Lift (Unsigned n) where
  lift (U i) = sigE [| fromIntegerU i |] (decUnsigned $ fromSing (sing :: (Sing n)))

decUnsigned :: Integer -> TypeQ
decUnsigned n = appT (conT ''Unsigned) (litT $ numTyLit n)

instance Show (Unsigned n) where
  show (U n) = show n

instance SingI n => Default (Unsigned n) where
  def = fromIntegerU 0

instance BitVector (Unsigned n) where
  type BitSize (Unsigned n) = n
  toBV   = toBitVector
  fromBV = fromBitVector

{-# NOINLINE toBitVector #-}
toBitVector :: SingI n => Unsigned n -> Vec n Bit
toBitVector (U m) = vreverse $ vmap (\x -> if odd x then H else L) $ viterate (`div` 2) m

{-# NOINLINE fromBitVector #-}
fromBitVector :: SingI n => Vec n Bit -> Unsigned n
fromBitVector = fromBitList . reverse . toList

{-# INLINABLE fromBitList #-}
fromBitList :: SingI n => [Bit] -> Unsigned n
fromBitList l = fromIntegerU_inlineable
              $ sum [ n
                    | (n,b) <- zip (iterate (*2) 1) l
                    , b == H
                    ]

{-# NOINLINE resizeU #-}
resizeU :: SingI m => Unsigned n -> Unsigned m
resizeU (U n) = fromIntegerU_inlineable n
