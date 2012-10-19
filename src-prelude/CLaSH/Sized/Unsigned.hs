{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module CLaSH.Sized.Unsigned
  ( Unsigned
  , resizeU
  )
where

import Data.Bits
import GHC.TypeLits

import CLaSH.Class.Default

newtype Unsigned (n :: Nat) = U Integer

instance Show (Unsigned n) where
  show (U n) = show n

instance Default (Unsigned n) where
  def = U 0

instance Eq (Unsigned n) where
  (U n) == (U m) = n == m

instance Ord (Unsigned n) where
  compare (U n) (U m) = compare n m

instance SingI n => Bounded (Unsigned n) where
  minBound = U 0
  maxBound = U $ (2 ^ fromSing (sing :: Sing n)) - 1

fromIntegerU :: forall n . SingI n => Integer -> Unsigned (n :: Nat)
fromIntegerU i = U $ i `mod` (2 ^ fromSing (sing :: Sing n))

plusU :: SingI n => Unsigned n -> Unsigned n -> Unsigned n
plusU (U a) (U b) = fromIntegerU $ a + b

minU :: SingI n => Unsigned n -> Unsigned n -> Unsigned n
minU (U a) (U b) = fromIntegerU $ a - b

timesU :: SingI n => Unsigned n -> Unsigned n -> Unsigned n
timesU (U a) (U b) = fromIntegerU $ a * b

negateU :: Unsigned n -> Unsigned n
negateU (U a) = (U a)

signumU :: Unsigned n -> Unsigned n
signumU (U 0) = (U 0)
signumU (U _) = (U 1)

instance SingI n => Num (Unsigned n) where
  (+)         = plusU
  (-)         = minU
  (*)         = timesU
  negate      = negateU
  abs         = error "'abs' undefined for Unsigned"
  signum      = signumU
  fromInteger = fromIntegerU

instance SingI n => Bits (Unsigned n) where
  (U a) .&. (U b)     = fromIntegerU (a .&. b)
  (U a) .|. (U b)     = fromIntegerU (a .|. b)
  xor (U a) (U b)     = fromIntegerU (xor a b)
  complement (U n)    = fromIntegerU (complement n)
  bit i               = fromIntegerU (bit i)
  testBit (U n) i     = testBit n i
  bitSizeMaybe _      = Just $ fromInteger (fromSing (sing :: Sing n))
  isSigned _          = False
  shiftL _ b | b < 0  = error "'shiftL'{Unsigned} undefined for negative numbers"
  shiftL (U n) b      = fromIntegerU (shiftL n b)
  shiftR _ b | b < 0  = error "'shiftR'{Unsigned} undefined for negative numbers"
  shiftR (U n) b      = fromIntegerU (shiftR n b)
  rotateL _ b | b < 0 = error "'shiftL'{Unsigned} undefined for negative numbers"
  rotateL n b         = let b' = b `mod` finiteBitSize n
                        in shiftL n b' .|. shiftR n (finiteBitSize n - b')
  rotateR _ b | b < 0 = error "'shiftR'{Unsigned} undefined for negative numbers"
  rotateR n b         = let b' = b `mod` finiteBitSize n
                        in shiftR n b' .|. shiftL n (finiteBitSize n - b')
  popCount (U n)      = popCount n

instance SingI n => FiniteBits (Unsigned n) where
  finiteBitSize _ = fromInteger $ fromSing (sing :: Sing n)

resizeU :: SingI m => Unsigned n -> Unsigned m
resizeU (U n) = fromIntegerU n
