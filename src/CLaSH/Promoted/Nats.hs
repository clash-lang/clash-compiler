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
