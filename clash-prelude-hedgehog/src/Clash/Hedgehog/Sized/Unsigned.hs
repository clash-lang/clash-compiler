{-|
Copyright   : (C) 2021-2022, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of Unsigned numbers.
-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}

module Clash.Hedgehog.Sized.Unsigned
  ( genUnsigned
  , SomeUnsigned(..)
  , genSomeUnsigned
  ) where

#if !MIN_VERSION_base(4,16,0)
import GHC.Natural (Natural)
#endif
import GHC.TypeNats
#if MIN_VERSION_base(4,18,0)
  hiding (SNat)
#endif
import Hedgehog (MonadGen, Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.Promoted.Nat
import Clash.Sized.Internal.Unsigned

genUnsigned :: forall n m. (MonadGen m, KnownNat n) => Range (Unsigned n) -> m (Unsigned n)
genUnsigned range =
  Gen.frequency
    [ (70, Gen.integral range)
    , (30, Gen.constant (Range.upperBound 99 range))
    ]

data SomeUnsigned atLeast where
  SomeUnsigned :: SNat n -> Unsigned (atLeast + n) -> SomeUnsigned atLeast

instance KnownNat atLeast => Show (SomeUnsigned atLeast) where
  show (SomeUnsigned SNat x) = show x

genSomeUnsigned
  :: forall atLeast m
   . (MonadGen m, KnownNat atLeast)
  => Range Natural
  -> m (SomeUnsigned atLeast)
genSomeUnsigned rangeUnsigned = do
  numExtra <- Gen.integral rangeUnsigned

  case someNatVal numExtra of
    SomeNat proxy -> SomeUnsigned (snatProxy proxy) <$> genUnsigned Range.linearBounded
