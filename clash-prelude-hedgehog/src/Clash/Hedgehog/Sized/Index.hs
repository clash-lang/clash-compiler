{-|
Copyright   : (C) 2021-2022, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of Index.
-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE GADTs #-}

module Clash.Hedgehog.Sized.Index
  ( genIndex
  , SomeIndex(..)
  , genSomeIndex
  ) where

import GHC.Natural (Natural)
import GHC.TypeNats
import Hedgehog (MonadGen, Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.Promoted.Nat
import Clash.Sized.Internal.Index

genIndex :: (MonadGen m, KnownNat n) => Range (Index n) -> m (Index n)
genIndex range =
  Gen.frequency
    [ (60, Gen.integral range)
    , (20, Gen.constant minBound)
    , (20, Gen.constant maxBound)
    ]

data SomeIndex atLeast where
  SomeIndex :: SNat n -> Index (atLeast + n) -> SomeIndex atLeast

instance KnownNat atLeast => Show (SomeIndex atLeast) where
  show (SomeIndex SNat ix) = show ix

genSomeIndex
  :: (MonadGen m, KnownNat atLeast)
  => Range Natural
  -> m (SomeIndex atLeast)
genSomeIndex rangeIx = do
  numExtra <- Gen.integral rangeIx

  case someNatVal numExtra of
    SomeNat proxy -> SomeIndex (snatProxy proxy) <$> genIndex Range.linearBounded
