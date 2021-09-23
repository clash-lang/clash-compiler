{-|
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of Unsigned numbers.
-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE GADTs #-}

module Clash.Hedgehog.Sized.Unsigned
  ( genUnsigned
  , SomeUnsigned(..)
  , genSomeUnsigned
  ) where

import GHC.Natural (Natural)
import GHC.TypeNats
import Hedgehog (MonadGen, Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.Promoted.Nat
import Clash.Sized.Internal.Unsigned

genUnsigned :: (MonadGen m, KnownNat n) => Range (Unsigned n) -> m (Unsigned n)
genUnsigned range =
  Gen.frequency
    [ (60, Gen.integral range)
    , (20, Gen.constant minBound)
    , (20, Gen.constant maxBound)
    ]

data SomeUnsigned atLeast where
  SomeUnsigned :: SNat n -> Unsigned (atLeast + n) -> SomeUnsigned atLeast

genSomeUnsigned
  :: (MonadGen m, KnownNat atLeast)
  => Range Natural
  -> m (SomeUnsigned atLeast)
genSomeUnsigned rangeUnsigned = do
  numExtra <- Gen.integral rangeUnsigned

  case someNatVal numExtra of
    SomeNat proxy -> SomeUnsigned (snatProxy proxy) <$> genUnsigned Range.linearBounded
