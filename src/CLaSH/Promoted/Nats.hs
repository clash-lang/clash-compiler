{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module CLaSH.Promoted.Nats where

import Data.Singletons
import GHC.TypeLits
import Unsafe.Coerce

data IsZero :: Nat -> * where
  IsZero :: IsZero 0
  IsSucc :: IsZero n -> IsZero (n + 1)

{-# NOINLINE isZero #-}
isZero :: Sing n -> IsZero n
isZero p = case fromSing p of
  0 -> unsafeCoerce IsZero
  n -> unsafeCoerce (IsSucc (isZero' (n - 1)))
  where
    isZero' :: Integer -> IsZero m
    isZero' 0 = unsafeCoerce IsZero
    isZero' m = unsafeCoerce (IsSucc (isZero' (m-1)))

addIsZero :: IsZero n -> IsZero m -> IsZero (n + m)
addIsZero IsZero     y      = y
addIsZero x          IsZero = x
addIsZero (IsSucc x) y      = IsSucc (addIsZero x y)

multIsZero :: IsZero n -> IsZero m -> IsZero (n * m)
multIsZero IsZero _      = IsZero
multIsZero _      IsZero = IsZero
multIsZero (IsSucc x)  y = addIsZero y (multIsZero x y)

powerIsZero :: IsZero n -> IsZero m -> IsZero (n ^ m)
powerIsZero _ IsZero     = IsSucc IsZero
powerIsZero x (IsSucc y) = multIsZero x (powerIsZero x y)
