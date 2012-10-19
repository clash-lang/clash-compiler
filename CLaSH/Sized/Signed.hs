{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module CLaSH.Sized.Signed
  ( Signed
  , resizeS
  )
where

import Data.Bits
import Language.Haskell.TH
import Language.Haskell.TH.Syntax(Lift(..))
import GHC.TypeLits

import CLaSH.Bit
import CLaSH.Class.Default
import CLaSH.Sized.VectorZ

newtype Signed (n :: Nat) = S Integer

instance SingI n => Lift (Signed n) where
  lift (S i) = sigE [| S i |] (decSigned $ fromSing (sing :: (Sing n)))

decSigned :: Integer -> TypeQ
decSigned n = appT (conT ''Signed) (litT $ numTyLit n)

instance Show (Signed n) where
  show (S n) = show n

instance Eq (Signed n) where
  (S n) == (S m) = n == m

instance Default (Signed n) where
  def = S 0

instance Ord (Signed n) where
  compare (S n) (S m) = compare n m

instance SingI n => Bounded (Signed n) where
  minBound = S $ negate $ 2 ^ (fromSing (sing :: Sing n) -1)
  maxBound = S $ 2 ^ (fromSing (sing :: Sing n) - 1) - 1

instance SingI n => Enum (Signed n) where
  toEnum         = fromIntegerS . toInteger
  fromEnum (S n) = fromEnum n

instance SingI n => Real (Signed n) where
  toRational (S n) = toRational n

instance SingI n => Integral (Signed n) where
  (S a) `quotRem` (S b) = let (a',b') = a `quotRem` b
                          in (fromIntegerS a', fromIntegerS b')
  toInteger (S n)       = n


fromIntegerS :: forall n . SingI n => Integer -> Signed (n :: Nat)
fromIntegerS v = res
  where
    sz' = 2 ^ (fromSing (sing :: Sing n) - 1)
    res = case divMod v sz' of
            (s,v') | even s    -> S v'
                   | otherwise -> S (v' - sz')

plusS :: SingI n => Signed n -> Signed n -> Signed n
plusS (S a) (S b) = fromIntegerS $ a + b

minS :: SingI n => Signed n -> Signed n -> Signed n
minS (S a) (S b) = fromIntegerS $ a - b

timesS :: SingI n => Signed n -> Signed n -> Signed n
timesS (S a) (S b) = fromIntegerS $ a * b

negateS :: SingI n => Signed n -> Signed n
negateS (S n) = fromIntegerS (0 - n)

absS :: SingI n => Signed n -> Signed n
absS (S n) = fromIntegerS (abs n)

signumS :: SingI n => Signed n -> Signed n
signumS (S n) = fromIntegerS (signum n)

instance SingI n => Num (Signed n) where
  (+)         = plusS
  (-)         = minS
  (*)         = timesS
  negate      = negateS
  abs         = absS
  signum      = signumS
  fromInteger = fromIntegerS

instance SingI n => Bits (Signed n) where
  (S a) .&. (S b)     = fromIntegerS (a .&. b)
  (S a) .|. (S b)     = fromIntegerS (a .|. b)
  xor (S a) (S b)     = fromIntegerS (xor a b)
  complement (S n)    = fromIntegerS (complement n)
  bit i               = fromIntegerS (bit i)
  testBit (S n) i     = testBit n i
  bitSizeMaybe _      = Just $ fromInteger (fromSing (sing :: Sing n))
  isSigned _          = True
  shiftL _ b | b < 0  = error "'shiftL'{Signed} undefined for negative numbers"
  shiftL (S n) b      = fromIntegerS (shiftL n b)
  shiftR _ b | b < 0  = error "'shiftR'{Signed} undefined for negative numbers"
  shiftR (S n) b      = fromIntegerS (shiftR n b)
  rotateL _ b | b < 0 = error "'shiftL'{Signed} undefined for negative numbers"
  rotateL n b         = let b' = b `mod` finiteBitSize n
                        in shiftL n b' .|. shiftR n (finiteBitSize n - b')
  rotateR _ b | b < 0 = error "'shiftR'{Signed} undefined for negative numbers"
  rotateR n b         = let b' = b `mod` finiteBitSize n
                        in shiftR n b' .|. shiftL n (finiteBitSize n - b')
  popCount (S n)      = popCount n

instance SingI n => FiniteBits (Signed n) where
  finiteBitSize _ = fromInteger $ fromSing (sing :: Sing n)

resizeS :: forall n m . (SingI n, SingI m) => Signed n -> Signed m
resizeS s@(S n) | n' <= m'  = fromIntegerS n
                | otherwise = fromBitList $ (take (m' - 1) l) ++ [last l]
  where
    n' = fromInteger $ fromSing (sing :: Sing n) :: Int
    m' = fromInteger $ fromSing (sing :: Sing m) :: Int
    l  = toList $ toBitVector s

toBitVector :: SingI n => Signed n -> Vec n Bit
toBitVector (S m) = vmap (\x -> if odd x then H else L) $ viterate (`div` 2) m

fromBitVector :: SingI n => Vec n Bit -> Signed n
fromBitVector = fromBitList . toList

fromBitList :: SingI n => [Bit] -> Signed n
fromBitList l = fromIntegerS
              $ sum [ n
                    | (n,b) <- zip (iterate (*2) 1) l
                    , b == H
                    ]
