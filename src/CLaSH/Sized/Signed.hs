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
import CLaSH.Sized.Vector

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

instance SingI n => Enum (Signed n) where
  succ           = plusS (fromIntegerS 1)
  pred           = minS (fromIntegerS 1)
  toEnum         = fromIntegerS . toInteger
  fromEnum       = fromEnum . toIntegerS

instance SingI n => Bounded (Signed n) where
  minBound = minBoundS
  maxBound = maxBoundS

minBoundS,maxBoundS :: forall n . SingI n => Signed n
{-# NOINLINE minBoundS #-}
minBoundS = S $ negate $ 2 ^ (fromSing (sing :: Sing n) -1)
{-# NOINLINE maxBoundS #-}
maxBoundS = S $ 2 ^ (fromSing (sing :: Sing n) - 1) - 1

instance SingI n => Num (Signed n) where
  (+)         = plusS
  (-)         = minS
  (*)         = timesS
  negate      = negateS
  abs         = absS
  signum      = signumS
  fromInteger = fromIntegerS

plusS,minS,timesS :: SingI n => Signed n -> Signed n -> Signed n
{-# NOINLINE plusS #-}
plusS (S a) (S b) = fromIntegerS_inlineable $ a + b

{-# NOINLINE minS #-}
minS (S a) (S b) = fromIntegerS_inlineable $ a - b

{-# NOINLINE timesS #-}
timesS (S a) (S b) = fromIntegerS_inlineable $ a * b

negateS,absS,signumS :: SingI n => Signed n -> Signed n
{-# NOINLINE negateS #-}
negateS (S n) = fromIntegerS_inlineable (0 - n)

{-# NOINLINE absS #-}
absS (S n) = fromIntegerS_inlineable (abs n)

{-# NOINLINE signumS #-}
signumS (S n) = fromIntegerS_inlineable (signum n)

fromIntegerS,fromIntegerS_inlineable :: forall n . SingI n => Integer -> Signed (n :: Nat)
{-# NOINLINE fromIntegerS #-}
fromIntegerS = fromIntegerS_inlineable
{-# INLINABLE fromIntegerS_inlineable #-}
fromIntegerS_inlineable i
    | nS == 0   = S 0
    | otherwise = res
  where
    nS  = fromSing (sing :: Sing n)
    sz  = 2 ^ (nS - 1)
    res = case divMod i sz of
            (s,i') | even s    -> S i'
                   | otherwise -> S (i' - sz)

instance SingI n => Real (Signed n) where
  toRational = toRational . toIntegerS

instance SingI n => Integral (Signed n) where
  quot      = quotS
  rem       = remS
  div       = divS
  mod       = modS
  quotRem   = quotRemS
  divMod    = divModS
  toInteger = toIntegerS

quotS,remS,divS,modS :: SingI n => Signed n -> Signed n -> Signed n
{-# NOINLINE quotS #-}
quotS = (fst.) . quotRemS_inlineable
{-# NOINLINE remS #-}
remS = (snd.) . quotRemS_inlineable
{-# NOINLINE divS #-}
divS = (fst.) . divModS_inlineable
{-# NOINLINE modS #-}
modS = (snd.) . divModS_inlineable

quotRemS,divModS :: SingI n => Signed n -> Signed n -> (Signed n, Signed n)
quotRemS n d = (n `quotS` d,n `remS` d)
divModS n d  = (n `divS` d,n `modS` d)

quotRemS_inlineable,divModS_inlineable :: SingI n => Signed n -> Signed n -> (Signed n, Signed n)
{-# INLINEABLE quotRemS_inlineable #-}
(S a) `quotRemS_inlineable` (S b) = let (a',b') = a `quotRem` b
                                    in (fromIntegerS_inlineable a', fromIntegerS_inlineable b')
{-# INLINEABLE divModS_inlineable #-}
(S a) `divModS_inlineable` (S b) = let (a',b') = a `divMod` b
                                   in (fromIntegerS_inlineable a', fromIntegerS_inlineable b')

{-# NOINLINE toIntegerS #-}
toIntegerS :: Signed n -> Integer
toIntegerS (S n) = n

instance SingI n => Bits (Signed n) where
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

andS,orS,xorS :: SingI n => Signed n -> Signed n -> Signed n
{-# NOINLINE andS #-}
(S a) `andS` (S b) = fromIntegerS_inlineable (a .&. b)
{-# NOINLINE orS #-}
(S a) `orS` (S b)  = fromIntegerS_inlineable (a .|. b)
{-# NOINLINE xorS #-}
(S a) `xorS` (S b) = fromIntegerS_inlineable (xor a b)

{-# NOINLINE complementS #-}
complementS :: SingI n => Signed n -> Signed n
complementS = fromBitVector . vmap complement . toBitVector

{-# NOINLINE bitS #-}
bitS :: SingI n => Int -> Signed n
bitS = fromIntegerS_inlineable . bit

{-# NOINLINE testBitS #-}
testBitS :: Signed n -> Int -> Bool
testBitS (S n) i = testBit n i

shiftLS,shiftRS,rotateLS,rotateRS :: SingI n => Signed n -> Int -> Signed n
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

instance SingI n => FiniteBits (Signed n) where
  finiteBitSize = finiteBitSizeS

{-# NOINLINE finiteBitSizeS #-}
finiteBitSizeS :: forall n . SingI n => Signed n -> Int
finiteBitSizeS _ = fromInteger $ fromSing (sing :: Sing n)

instance Show (Signed n) where
  show (S n) = show n

instance SingI n => Default (Signed n) where
  def = fromIntegerS 0

instance SingI n => Lift (Signed n) where
  lift (S i) = sigE [| fromIntegerS i |] (decSigned $ fromSing (sing :: (Sing n)))

decSigned :: Integer -> TypeQ
decSigned n = appT (conT ''Signed) (litT $ numTyLit n)

instance BitVector (Signed n) where
  type BitSize (Signed n) = n
  toBV   = toBitVector
  fromBV = fromBitVector

{-# NOINLINE toBitVector #-}
toBitVector :: SingI n => Signed n -> Vec n Bit
toBitVector (S m) = vreverse $ vmap (\x -> if odd x then H else L) $ viterateI (`div` 2) m

{-# NOINLINE fromBitVector #-}
fromBitVector :: SingI n => Vec n Bit -> Signed n
fromBitVector = fromBitList . reverse . toList

{-# INLINABLE fromBitList #-}
fromBitList :: SingI n => [Bit] -> Signed n
fromBitList l = fromIntegerS_inlineable
              $ sum [ n
                    | (n,b) <- zip (iterate (*2) 1) l
                    , b == H
                    ]

{-# NOINLINE resizeS #-}
resizeS :: forall n m . (SingI n, SingI m) => Signed n -> Signed m
resizeS s@(S n) | n' <= m'  = fromIntegerS_inlineable n
                | otherwise = case l of
                    (x:xs) -> fromBitList $ reverse $ x : (drop (n' - m') xs)
                    _      -> error "resizeS impossible case: empty list"
  where
    n' = fromInteger $ fromSing (sing :: Sing n) :: Int
    m' = fromInteger $ fromSing (sing :: Sing m) :: Int
    l  = toList $ toBitVector s
