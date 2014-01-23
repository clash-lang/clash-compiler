{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module CLaSH.Promoted.Nat
  ( SNat, snat, withSNat, fromSNat
  , UNat (..), toUNat, addUNat, multUNat, powUNat
  )
where

import Data.Proxy
import GHC.TypeLits
import Unsafe.Coerce

data SNat (n :: Nat) = KnownNat n => SNat (Proxy n)

snat :: KnownNat n => SNat n
snat = SNat Proxy

withSNat :: KnownNat n => (SNat n -> a) -> a
withSNat f = f (SNat Proxy)

data UNat :: Nat -> * where
  UZero :: UNat 0
  USucc :: UNat n -> UNat (n + 1)

fromSNat :: SNat n -> Integer
fromSNat (SNat p) = natVal p

{-# NOINLINE fromSNat #-}
toUNat :: SNat n -> UNat n
toUNat (SNat p) = fromI (natVal p)
  where
    fromI :: Integer -> UNat m
    fromI 0 = unsafeCoerce UZero
    fromI n = unsafeCoerce (USucc (fromI (n - 1)))

addUNat :: UNat n -> UNat m -> UNat (n + m)
addUNat UZero     y     = y
addUNat x         UZero = x
addUNat (USucc x) y     = USucc (addUNat x y)

multUNat :: UNat n -> UNat m -> UNat (n * m)
multUNat UZero      _     = UZero
multUNat _          UZero = UZero
multUNat (USucc x) y      = addUNat y (multUNat x y)

powUNat :: UNat n -> UNat m -> UNat (n ^ m)
powUNat _ UZero     = USucc UZero
powUNat x (USucc y) = multUNat x (powUNat x y)
