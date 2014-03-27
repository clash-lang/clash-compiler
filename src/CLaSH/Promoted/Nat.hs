{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module CLaSH.Promoted.Nat
  ( SNat, snat, withSNat
  , UNat (..), toUNat, addUNat, multUNat, powUNat
  )
where

import Data.Proxy
import GHC.TypeLits
import Unsafe.Coerce

-- | Singleton value for a type-level natural number 'n'
data SNat (n :: Nat) = KnownNat n => SNat (Proxy n)

instance Show (SNat n) where
  show (SNat p) = 'd' : show (natVal p)

-- | Singleton value for a type-level natural number
snat :: KnownNat n => SNat n
snat = SNat Proxy

-- | Supply a function with a singleton natural 'n' according to the context
withSNat :: KnownNat n => (SNat n -> a) -> a
withSNat f = f (SNat Proxy)

-- | Unary representation of a type-level natural
data UNat :: Nat -> * where
  UZero :: UNat 0
  USucc :: UNat n -> UNat (n + 1)

-- | Convert a singleton natural number to it's unary representation
toUNat :: SNat n -> UNat n
toUNat (SNat p) = fromI (natVal p)
  where
    fromI :: Integer -> UNat m
    fromI 0 = unsafeCoerce UZero
    fromI n = unsafeCoerce (USucc (fromI (n - 1)))

-- | Add two singleton natural numbers
addUNat :: UNat n -> UNat m -> UNat (n + m)
addUNat UZero     y     = y
addUNat x         UZero = x
addUNat (USucc x) y     = unsafeCoerce (USucc (addUNat x y))

-- | Multiply two singleton natural numbers
multUNat :: UNat n -> UNat m -> UNat (n * m)
multUNat UZero      _     = UZero
multUNat _          UZero = UZero
multUNat (USucc x) y      = unsafeCoerce (addUNat y (multUNat x y))

-- | Exponential of two singleton natural numbers
powUNat :: UNat n -> UNat m -> UNat (n ^ m)
powUNat _ UZero     = USucc UZero
powUNat x (USucc y) = unsafeCoerce (multUNat x (powUNat x y))
