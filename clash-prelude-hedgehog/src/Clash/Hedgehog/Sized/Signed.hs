{-|
Copyright   : (C) 2021-2022, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of Signed numbers.
-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}

module Clash.Hedgehog.Sized.Signed
  ( genSigned
  , SomeSigned(..)
  , genSomeSigned
  ) where

#if !MIN_VERSION_base(4,16,0)
import GHC.Natural (Natural)
#endif
import GHC.TypeNats
import Hedgehog (MonadGen, Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.Promoted.Nat
import Clash.Sized.Internal.Signed

genSigned :: (MonadGen m, KnownNat n) => Range (Signed n) -> m (Signed n)
genSigned range =
  Gen.frequency
    [ (60, Gen.integral range)
    , (20, Gen.constant (Range.lowerBound 99 range))
    , (20, Gen.constant (Range.upperBound 99 range))
    ]

data SomeSigned atLeast where
  SomeSigned :: SNat n -> Signed (atLeast + n) -> SomeSigned atLeast

instance KnownNat atLeast => Show (SomeSigned atLeast) where
  show (SomeSigned SNat x) = show x

genSomeSigned
  :: (MonadGen m, KnownNat atLeast)
  => Range Natural
  -> m (SomeSigned atLeast)
genSomeSigned rangeSigned = do
  numExtra <- Gen.integral rangeSigned

  case someNatVal numExtra of
    SomeNat proxy -> SomeSigned (snatProxy proxy) <$> genSigned Range.linearBounded
