{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiWayIf #-}
module CLaSH.Sized.Signed
  ( Signed
  , resize
  )
where

import Data.Bits
import GHC.TypeLits

newtype Signed (n :: Nat) = S Integer

instance Show (Signed n) where
  show (S n) = show n

instance Eq (Signed n) where
  (S n) == (S m) = n == m

instance Ord (Signed n) where
  compare (S n) (S m) = compare n m

fromIntegerS :: forall n . SingI n => Integer -> Signed (n :: Nat)
fromIntegerS i = let n' = 2 ^ (fromSing (sing :: Sing n) - 1)
                 in case i of j | j < 0     -> undefined
                                | j > n'    -> undefined
                                | otherwise -> S $ i `mod` n'


instance SingI n => Num (Signed n) where
  (+) = undefined
  (-) = undefined
  (*) = undefined
  negate = undefined
  abs = undefined
  signum = undefined
  fromInteger = fromIntegerS

resize :: Signed n -> Signed m
resize _ = undefined
