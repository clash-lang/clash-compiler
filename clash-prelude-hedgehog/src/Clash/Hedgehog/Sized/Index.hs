{-|
Copyright   : (C) 2021-2022, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of Index.
-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}

module Clash.Hedgehog.Sized.Index
  ( genIndex
  , SomeIndex(..)
  , genSomeIndex
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
import Clash.Sized.Internal.Index

genIndex :: forall n m. (MonadGen m, KnownNat n) => Range (Index n) -> m (Index n)
genIndex range =
  Gen.frequency
    [ (70, Gen.integral range)
    , (30, Gen.constant (Range.upperBound 99 range))
    ]

data SomeIndex atLeast where
  SomeIndex :: SNat n -> Index (atLeast + n) -> SomeIndex atLeast

instance KnownNat atLeast => Show (SomeIndex atLeast) where
  show (SomeIndex SNat ix) = show ix

genSomeIndex
  :: forall atLeast m
  . (MonadGen m, KnownNat atLeast)
  => Range Natural
  -> m (SomeIndex atLeast)
genSomeIndex rangeIx = do
  numExtra <- Gen.integral rangeIx

  case someNatVal numExtra of
    SomeNat proxy -> SomeIndex (snatProxy proxy) <$> genIndex Range.linearBounded
