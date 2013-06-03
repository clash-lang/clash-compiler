{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
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
import CLaSH.Class.BitVector
import CLaSH.Class.Default
import CLaSH.Sized.VectorZ

newtype Signed (n :: Nat) = S Integer

instance SingI n => Lift (Signed n) where
  lift (S i) = sigE [| fromInteger i |] (decSigned $ fromSing (sing :: (Sing n)))

decSigned :: Integer -> TypeQ
decSigned n = appT (conT ''Signed) (litT $ numTyLit n)

instance Show (Signed n) where
  show (S n) = show n

instance Eq (Signed n) where
  (==) = eqS

{-# NOINLINE eqS #-}
eqS :: (Signed n) -> (Signed n) -> Bool
(S n) `eqS` (S m) = n == m

instance SingI n => Default (Signed n) where
  def = fromIntegerS 0

instance Ord (Signed n) where
  compare (S n) (S m) = compare n m
  (<)  = ltS
  (>=) = geS
  (>)  = gtS
  (<=) = leS

{-# NOINLINE ltS #-}
ltS :: Signed n -> Signed n -> Bool
ltS (S n) (S m) = n < m
{-# NOINLINE geS #-}
geS :: Signed n -> Signed n -> Bool
geS (S n) (S m) = n >= m
{-# NOINLINE gtS #-}
gtS :: Signed n -> Signed n -> Bool
gtS (S n) (S m) = n > m
{-# NOINLINE leS #-}
leS :: Signed n -> Signed n -> Bool
leS (S n) (S m) = n <= m

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

{-# NOINLINE fromIntegerS #-}
fromIntegerS :: forall n . SingI n => Integer -> Signed (n :: Nat)
fromIntegerS i = res
  where
    sz' = 2 ^ (fromSing (sing :: Sing n) - 1)
    res = case divMod i sz' of
            (s,i') | even s    -> S i'
                   | otherwise -> S (i' - sz')

{-# NOINLINE plusS #-}
plusS :: SingI n => Signed n -> Signed n -> Signed n
plusS (S a) (S b) = fromIntegerS $ a + b

{-# NOINLINE minS #-}
minS :: SingI n => Signed n -> Signed n -> Signed n
minS (S a) (S b) = fromIntegerS $ a - b

{-# NOINLINE timesS #-}
timesS :: SingI n => Signed n -> Signed n -> Signed n
timesS (S a) (S b) = fromIntegerS $ a * b

{-# NOINLINE negateS #-}
negateS :: SingI n => Signed n -> Signed n
negateS (S n) = fromIntegerS (0 - n)

{-# NOINLINE absS #-}
absS :: SingI n => Signed n -> Signed n
absS (S n) = fromIntegerS (abs n)

{-# NOINLINE signumS #-}
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

{-# NOINLINE resizeS #-}
resizeS :: forall n m . (SingI n, SingI m) => Signed n -> Signed m
resizeS s@(S n) | n' <= m'  = fromIntegerS n
                | otherwise = fromBitList $ (take (m' - 1) l) ++ [last l]
  where
    n' = fromInteger $ fromSing (sing :: Sing n) :: Int
    m' = fromInteger $ fromSing (sing :: Sing m) :: Int
    l  = toList $ toBitVector s

{-# NOINLINE toBitVector #-}
toBitVector :: SingI n => Signed n -> Vec n Bit
toBitVector (S m) = vreverse $ vmap (\x -> if odd x then H else L) $ viterate (`div` 2) m

{-# NOINLINE fromBitVector #-}
fromBitVector :: SingI n => Vec n Bit -> Signed n
fromBitVector = fromBitList . reverse . toList

instance BitVector (Signed n) where
  type BitSize (Signed n) = n
  toBV   = toBitVector
  fromBV = fromBitVector

fromBitList :: SingI n => [Bit] -> Signed n
fromBitList l = fromIntegerS
              $ sum [ n
                    | (n,b) <- zip (iterate (*2) 1) l
                    , b == H
                    ]
