{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE UndecidableInstances #-}

module GenericBitPackTypes where

import Clash.Prelude

--type MyTuple = (Int, Int, Char)

-- | Product type
data FooProduct a b
  = FooProduct a b
    deriving Generic

instance ( BitPack a
         , BitPack b
         , KnownNat (BitSize a)
         , KnownNat (BitSize b)
         ) => BitPack (FooProduct a b)


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

instance ( BitPack a
         , BitPack b
         , KnownNat (BitSize a)
         , KnownNat (BitSize b)
         ) => BitPack (FooSP1 a b)
--
-- | Foo sum-of-products non-aligned
data FooSP2 a b
  = FooSP2_AB a b
  | FooSP2_A a
  | FooSP2_B b
    deriving (Generic)


instance ( BitPack a
         , BitPack b
         , KnownNat (BitSize a)
         , KnownNat (BitSize b)
         ) => BitPack (FooSP2 a b)
