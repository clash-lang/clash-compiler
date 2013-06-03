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

instance forall n . SingI n => Lift (Unsigned n) where
  lift (U i) = sigE [| fromInteger i |] (decUnsigned $ fromSing (sing :: (Sing n)))

decUnsigned :: Integer -> TypeQ
decUnsigned n = appT (conT ''Unsigned) (litT $ numTyLit n)

instance Show (Unsigned n) where
  show (U n) = show n

instance SingI n => Default (Unsigned n) where
  def = fromIntegerU 0

instance Eq (Unsigned n) where
  (==) = eqU

{-# NOINLINE eqU #-}
eqU :: (Unsigned n) -> (Unsigned n) -> Bool
(U n) `eqU` (U m) = n == m

instance Ord (Unsigned n) where
  compare (U n) (U m) = compare n m

instance SingI n => Bounded (Unsigned n) where
  minBound = U 0
  maxBound = U $ (2 ^ fromSing (sing :: Sing n)) - 1

{-# NOINLINE fromIntegerU #-}
fromIntegerU :: forall n . SingI n => Integer -> Unsigned (n :: Nat)
fromIntegerU i = U $ i `mod` (2 ^ fromSing (sing :: Sing n))

{-# NOINLINE plusU #-}
plusU :: SingI n => Unsigned n -> Unsigned n -> Unsigned n
plusU (U a) (U b) = fromIntegerU $ a + b

{-# NOINLINE minU #-}
minU :: SingI n => Unsigned n -> Unsigned n -> Unsigned n
minU (U a) (U b) = fromIntegerU $ a - b

{-# NOINLINE timesU #-}
timesU :: SingI n => Unsigned n -> Unsigned n -> Unsigned n
timesU (U a) (U b) = fromIntegerU $ a * b

{-# NOINLINE negateU #-}
negateU :: Unsigned n -> Unsigned n
negateU (U a) = (U a)

{-# NOINLINE signumU #-}
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

{-# NOINLINE toBitVector #-}
toBitVector :: SingI n => Unsigned n -> Vec n Bit
toBitVector (U m) = vreverse $ vmap (\x -> if odd x then H else L) $ viterate (`div` 2) m

{-# NOINLINE fromBitVector #-}
fromBitVector :: SingI n => Vec n Bit -> Unsigned n
fromBitVector = fromBitList . reverse . toList

instance BitVector (Unsigned n) where
  type BitSize (Unsigned n) = n
  toBV   = toBitVector
  fromBV = fromBitVector

fromBitList :: SingI n => [Bit] -> Unsigned n
fromBitList l = fromIntegerU
              $ sum [ n
                    | (n,b) <- zip (iterate (*2) 1) l
                    , b == H
                    ]
