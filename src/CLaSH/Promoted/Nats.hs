{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module CLaSH.Promoted.Nats where

import GHC.TypeLits
import Unsafe.Coerce

data SNat (n :: Nat) = SNat

data IsZero :: Nat -> * where
  IsZero :: IsZero 0
  IsSucc :: IsZero n -> IsZero (n + 1)

isZero :: KnownNat n => SNat n -> IsZero n
isZero p = case natVal p of
  0 -> unsafeCoerce IsZero
  n -> unsafeCoerce (IsSucc (isZero' (n - 1)))
  where
    isZero' :: Integer -> IsZero m
    isZero' 0 = unsafeCoerce IsZero
    isZero' m = unsafeCoerce (IsSucc (isZero' (m-1)))

withSNat :: forall n b . KnownNat n => (SNat n -> b) -> b
withSNat f = f (SNat :: SNat n)
