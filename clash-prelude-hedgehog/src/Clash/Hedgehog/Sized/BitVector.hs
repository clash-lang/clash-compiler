{-|
Copyright   : (C) 2021-2022, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of BitVector.
-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE CPP #-}
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

#if !MIN_VERSION_base(4,16,0)
import GHC.Natural (Natural)
#endif
import GHC.TypeNats
import Hedgehog (MonadGen, Range)
import Hedgehog.Internal.Range (constantBounded, constant)
import qualified Hedgehog.Gen as Gen

import Clash.Class.BitPack (pack)
import Clash.Promoted.Nat
import Clash.Sized.Internal.BitVector
import Clash.XException (errorX)

import Clash.Hedgehog.Sized.Unsigned

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
genDefinedBitVector = pack <$> genUnsigned constantBounded

-- | Generate a bit vector where some bits may be undefined.
--
genBitVector :: forall m n . (MonadGen m, KnownNat n) => m (BitVector n)
genBitVector =
  Gen.frequency
    [ (70, BV <$> genNatural <*> genNatural)
    , (10, Gen.constant minBound)
    , (10, Gen.constant maxBound)
    , (10, Gen.constant undefined#)
    ]
 where
  genNatural = Gen.integral $ constant 0 (2^natToNatural @n)

data SomeBitVector atLeast where
  SomeBitVector :: SNat n -> BitVector (atLeast + n) -> SomeBitVector atLeast

instance KnownNat atLeast => Show (SomeBitVector atLeast) where
  show (SomeBitVector SNat bv) = show bv

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
