{-|
Copyright   : (C) 2021-2022, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of RTree.
-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}

module Clash.Hedgehog.Sized.RTree
  ( genRTree
  , genNonEmptyRTree
  , SomeRTree(..)
  , genSomeRTree
  ) where

#if !MIN_VERSION_base(4,16,0)
import GHC.Natural (Natural)
#endif
import GHC.TypeNats
import Hedgehog (MonadGen, Range)
import qualified Hedgehog.Gen as Gen

import Clash.Promoted.Nat
import Clash.Sized.RTree

genRTree :: (MonadGen m, KnownNat n) => m a -> m (RTree n a)
genRTree genElem = sequenceA (trepeat genElem)

genNonEmptyRTree :: (MonadGen m, KnownNat n, 1 <= n) => m a -> m (RTree n a)
genNonEmptyRTree = genRTree

data SomeRTree atLeast a where
  SomeRTree :: SNat n -> RTree (atLeast + n) a -> SomeRTree atLeast a

instance (KnownNat atLeast, Show a) => Show (SomeRTree atLeast a) where
  show (SomeRTree SNat x) = show x

genSomeRTree
  :: (MonadGen m, KnownNat atLeast)
  => Range Natural
  -> m a
  -> m (SomeRTree atLeast a)
genSomeRTree rangeElems genElem = do
  numExtra <- Gen.integral rangeElems

  case someNatVal numExtra of
    SomeNat proxy -> SomeRTree (snatProxy proxy) <$> genRTree genElem
