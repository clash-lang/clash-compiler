{-|
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of BitVector.
-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Hedgehog.Sized.BitVector
  ( genDefinedBit
  , genBit
  , genDefinedBitVector
  , genBitVector
  , SomeBitVector(..)
  , genSomeBitVector
  ) where

import GHC.Natural (Natural)
import GHC.TypeNats
import Hedgehog (MonadGen, Range)
import qualified Hedgehog.Gen as Gen

import Clash.Promoted.Nat
import Clash.Sized.Internal.BitVector
import Clash.Sized.Vector (v2bv)
import Clash.XException (errorX)

import Clash.Hedgehog.Sized.Vector (genVec)

-- | Generate a bit which is guaranteed to be defined.
-- This will either have the value 'low' or 'high'.
--
genDefinedBit :: (MonadGen m) => m Bit
genDefinedBit = Gen.element [low, high]

-- | Generate a bit which is not guaranteed to be defined.
-- This will either have the value 'low' or 'high', or throw an 'XException'.
--
genBit :: (MonadGen m) => m Bit
genBit = Gen.element [low, high, errorX "X"]

-- | Generate a bit vector where all bits are defined.
--
genDefinedBitVector :: (MonadGen m, KnownNat n) => m (BitVector n)
genDefinedBitVector =
  Gen.frequency
    [ (60, fmap v2bv (genVec genDefinedBit))
    , (20, Gen.constant minBound)
    , (20, Gen.constant maxBound)
    ]

-- | Generate a bit vector where some bits may be undefined.
--
genBitVector :: (MonadGen m, KnownNat n) => m (BitVector n)
genBitVector =
  Gen.frequency
    [ (55, fmap v2bv (genVec genBit))
    , (15, Gen.constant minBound)
    , (15, Gen.constant maxBound)
    , (15, Gen.constant undefined#)
    ]

data SomeBitVector atLeast where
  SomeBitVector :: SNat n -> BitVector (atLeast + n) -> SomeBitVector atLeast

genSomeBitVector
  :: forall m atLeast
   . (MonadGen m, KnownNat atLeast)
  => Range Natural
  -> (forall n. KnownNat n => m (BitVector n))
  -> m (SomeBitVector atLeast)
genSomeBitVector rangeBv genBv = do
  numExtra <- Gen.integral rangeBv

  case someNatVal numExtra of
    SomeNat proxy -> SomeBitVector (snatProxy proxy) <$> genBv
