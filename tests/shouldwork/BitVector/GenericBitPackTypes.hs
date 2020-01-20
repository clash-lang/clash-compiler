{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}

module GenericBitPackTypes where

import Clash.Prelude

--type MyTuple = (Int, Int, Char)

-- | Product type
data FooProduct a b
  = FooProduct a b
    deriving Generic

instance (BitPack a, BitPack b) => BitPack (FooProduct a b)


-- | Sum type
data FooSum
  = FooSumA
  | FooSumB
  | FooSumC
  | FooSumD
  | FooSumE
  | FooSumF
  | FooSumG
    deriving (Generic, BitPack)

-- | Foo sum-of-products aligned
data FooSP1 a b
  = FooSP1_AB a b
  | FooSP1_BA b a
    deriving (Generic)

instance (BitPack a, BitPack b) => BitPack (FooSP1 a b)
--
-- | Foo sum-of-products non-aligned
data FooSP2 a b
  = FooSP2_AB a b
  | FooSP2_A a
  | FooSP2_B b
    deriving (Generic)


instance (BitPack a, BitPack b) => BitPack (FooSP2 a b)

-- Testsuite data (in separate module to circumvent TH stage restrictions):
type U1 = Unsigned 3
type U2 = Unsigned 5

aT :: FooProduct U1 U2
aT = FooProduct 1 2

bT = FooSumA
cT = FooSumB
dT = FooSumC
eT = FooSumG

fT :: FooSP1 U1 U2
fT = FooSP1_AB 1 2

gT :: FooSP1 U1 U2
gT = FooSP1_BA 1 2

hT :: FooSP2 U1 U2
hT = FooSP2_AB 2 1

iT :: FooSP2 U1 U2
iT = FooSP2_A 2


jT :: FooSP2 U1 U2
jT = FooSP2_B 1

kT :: (U1, U2)
kT = (1, 2)
