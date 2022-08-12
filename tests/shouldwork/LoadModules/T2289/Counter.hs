{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC
    -O
    -fno-worker-wrapper
    -fexpose-all-unfoldings
    -fplugin GHC.TypeLits.Extra.Solver
    -fplugin GHC.TypeLits.Normalise
    -fplugin GHC.TypeLits.KnownNat.Solver
    -fconstraint-solver-iterations=10 #-}

module T2289.Counter(CounterOverflow, Counter, mkCounter, tick, hasOverflown) where

import Clash.Prelude
import Data.Constraint ( type (:-)(..), Dict(..) )
import Unsafe.Coerce (unsafeCoerce)

-- | A counter with n counts and o overflow capacity
-- This counter will overflow after n - 1 tick calls
-- and remember it for at least o ticks.
--
-- Ex: CounterOverflow 2 5 will overflow after calling tick once
--     and it will remember that overflow for at least 4 ticks.
newtype CounterOverflow (n :: Nat) (o :: Nat)
  = CounterOverflow (Unsigned (1 + CLog 2 (Max n o)))
  deriving (Eq, Ord, Generic, NFDataX)

-- | simple counter that can tick to (n - 1) and remembers that overflow for
--   at least (n - 1) which has the optimal underlying counter efficiency.
type Counter (n :: Nat) = CounterOverflow n n

deriving instance (KnownNat n, KnownNat o, 1 <= Max n o) => BitPack (CounterOverflow n o)

instance (KnownNat n, KnownNat o, 1 <= n) => Show (CounterOverflow n o) where
  show (CounterOverflow n) = case maxAxiomL @1 @n @o of
                               Sub Dict -> "CounterOverflow " <> show (n - initCount @0 @n @o)

clogAxiom
  :: forall (n :: Nat) (p :: Nat) (b :: Nat)
   . (1 <= n, p <= n) :- (p <= b ^ CLog b n)
clogAxiom = Sub (unsafeCoerce (Dict :: Dict (a ~ a)))

maxAxiomL
  :: forall (n :: Nat) (p :: Nat) (q :: Nat)
   . (n <= p) :- (n <= Max p q)
maxAxiomL = Sub (unsafeCoerce (Dict :: Dict (a ~ a)))

initCount
  :: forall (init :: Nat)
          (n :: Nat)
          (o :: Nat)
   . KnownNat n
  => KnownNat o
  => KnownNat init
  => 1 <= n
  => Unsigned (1 + CLog 2 (Max n o))
initCount = case maxAxiomL @1 @n @o of
              Sub Dict -> case clogAxiom @(Max n o) @n @2 of
                Sub Dict -> natToNum @(2 ^ CLog 2 (Max n o) - n + 1 + init)

mkCounter
  :: forall (n :: Nat)
            (o :: Nat)
   . KnownNat n
  => KnownNat o
  => 1 <= n
  => CounterOverflow n o
mkCounter = CounterOverflow (initCount @0 @n @o)

-- | Increase the counter by one
tick
  :: forall (n :: Nat)
            (o :: Nat)
   . KnownNat n
  => KnownNat o
  => 1 <= n
  => CounterOverflow n o
  -> CounterOverflow n o
tick (CounterOverflow i) = case maxAxiomL @1 @n @o of
                             Sub Dict -> CounterOverflow (i + 1)

-- | Test whether the counter has reached its maximum
hasOverflown
  :: forall (n :: Nat)
            (o :: Nat)
   . KnownNat n
  => KnownNat o
  => 1 <= n
  => CounterOverflow n o
  -> Bool
hasOverflown (CounterOverflow i) = case maxAxiomL @1 @n @o of
                                     Sub Dict -> bitToBool (msb i)
