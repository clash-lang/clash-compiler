{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module CLaSH.Sized.Index
  ( Index
  )
where

import Data.Default
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Syntax(Lift(..))
import GHC.TypeLits

import CLaSH.Class.Num

-- | Arbitrary-bounded unsigned integer represented by @ceil(log(n))@ bits
--
-- Given an upper bound @n@, an 'Index' @n@ number has a range of: [0 .. @n@-1]
newtype Index (n :: Nat) = I Integer
  deriving Typeable

instance Eq (Index n) where
  (==) = eqI

{-# NOINLINE eqI #-}
eqI :: (Index n) -> (Index n) -> Bool
(I n) `eqI` (I m) = n == m

instance Ord (Index n) where
  (<)  = ltI
  (>=) = geI
  (>)  = gtI
  (<=) = leI

ltI,geI,gtI,leI :: Index n -> Index n -> Bool
{-# NOINLINE ltI #-}
ltI (I n) (I m) = n < m
{-# NOINLINE geI #-}
geI (I n) (I m) = n >= m
{-# NOINLINE gtI #-}
gtI (I n) (I m) = n > m
{-# NOINLINE leI #-}
leI (I n) (I m) = n <= m

instance KnownNat n => Enum (Index n) where
  succ           = plusI (fromIntegerI 1)
  pred           = minI (fromIntegerI 1)
  toEnum         = fromIntegerI . toInteger
  fromEnum       = fromEnum . toIntegerI
  enumFrom       = enumFromI
  enumFromThen   = enumFromThenI
  enumFromTo     = enumFromToI
  enumFromThenTo = enumFromThenToI

{-# NOINLINE enumFromI #-}
{-# NOINLINE enumFromThenI #-}
{-# NOINLINE enumFromToI #-}
{-# NOINLINE enumFromThenToI #-}
enumFromI       :: KnownNat n => Index n -> [Index n]
enumFromThenI   :: KnownNat n => Index n -> Index n -> [Index n]
enumFromToI     :: KnownNat n => Index n -> Index n -> [Index n]
enumFromThenToI :: KnownNat n => Index n -> Index n -> Index n -> [Index n]
enumFromI x             = map toEnum [fromEnum x ..]
enumFromThenI x y       = map toEnum [fromEnum x, fromEnum y ..]
enumFromToI x y         = map toEnum [fromEnum x .. fromEnum y]
enumFromThenToI x1 x2 y = map toEnum [fromEnum x1, fromEnum x2 .. fromEnum y]

instance KnownNat n => Bounded (Index n) where
  minBound = fromIntegerI 0
  maxBound = maxBoundI

{-# NOINLINE maxBoundI #-}
maxBoundI :: KnownNat n => Index n
maxBoundI = let res = I (natVal res - 1) in res

-- | Operators do @wrap-around@ on overflow
instance KnownNat n => Num (Index n) where
  (+)         = plusI
  (-)         = minI
  (*)         = timesI
  negate      = id
  abs         = id
  signum      = signumI
  fromInteger = fromIntegerI

plusI,minI,timesI :: KnownNat n => Index n -> Index n -> Index n
{-# NOINLINE plusI #-}
plusI (I a) (I b) = fromIntegerI_inlineable $ a + b

{-# NOINLINE minI #-}
minI (I a) (I b) = fromIntegerI_inlineable $ a - b

{-# NOINLINE timesI #-}
timesI (I a) (I b) = fromIntegerI_inlineable $ a * b

{-# NOINLINE signumI #-}
signumI :: Index n -> Index n
signumI (I 0) = (I 0)
signumI (I _) = (I 1)

fromIntegerI,fromIntegerI_inlineable :: KnownNat n => Integer -> Index n
{-# NOINLINE fromIntegerI #-}
fromIntegerI = fromIntegerI_inlineable
{-# INLINABLE fromIntegerI_inlineable #-}
fromIntegerI_inlineable i =
  let bound = natVal res
      i'    = i `mod` bound
      err   = error (show i ++ " is out of bounds: [0.." ++ show (bound - 1) ++ "]")
      res   = if i' /= i then err else I i
  in  res

instance KnownNat n => Real (Index n) where
  toRational = toRational . toIntegerI

instance KnownNat n => Integral (Index n) where
  quot      = quotI
  rem       = remI
  div       = quotI
  mod       = modI
  quotRem   = quotRemI
  divMod    = divModI
  toInteger = toIntegerI

quotI,remI,modI :: KnownNat n => Index n -> Index n -> Index n
{-# NOINLINE quotI #-}
quotI = (fst.) . quotRemI_inlineable
{-# NOINLINE remI #-}
remI = (snd.) . quotRemI_inlineable
{-# NOINLINE modI #-}
(I a) `modI` (I b) = fromIntegerI_inlineable (a `mod` b)

quotRemI,divModI :: KnownNat n => Index n -> Index n -> (Index n, Index n)
quotRemI n d = (n `quotI` d,n `remI` d)
divModI n d  = (n `quotI` d,n `modI` d)

{-# INLINEABLE quotRemI_inlineable #-}
quotRemI_inlineable :: KnownNat n => Index n -> Index n -> (Index n, Index n)
(I a) `quotRemI_inlineable` (I b) = let (a',b') = a `quotRem` b
                                    in (fromIntegerI_inlineable a', fromIntegerI_inlineable b')

{-# NOINLINE toIntegerI #-}
toIntegerI :: Index n -> Integer
toIntegerI (I n) = n

instance KnownNat n => Lift (Index n) where
  lift u@(I i) = sigE [| fromIntegerI i |] (decIndex (natVal u))

decIndex :: Integer -> TypeQ
decIndex n = appT (conT ''Index) (litT $ numTyLit n)

instance Show (Index n) where
  show (I n) = show n

instance KnownNat n => Default (Index n) where
  def = fromIntegerI 0

{-# NOINLINE resizeI #-}
resizeI :: KnownNat m => Index n -> Index m
resizeI (I n) = fromIntegerI_inlineable n

-- | A resize operation that zero-extends on extension, and wraps on truncation.
--
-- Increasing the size of the number extends with zeros to the left.
-- Truncating a number of length N to a length L just removes the left
-- (most significant) N-L bits.
instance Resize Index where
  resize = resizeI
