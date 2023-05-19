{-|
Copyright   : (C) 2021-2022, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of vectors.
-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}

module Clash.Hedgehog.Sized.Vector
  ( genVec
  , genNonEmptyVec
  , SomeVec(..)
  , genSomeVec
  ) where

import Prelude hiding (repeat)

#if !MIN_VERSION_base(4,16,0)
import GHC.Natural (Natural)
#endif
import GHC.TypeNats
#if MIN_VERSION_base(4,18,0)
  hiding (SNat)
#endif
import Hedgehog (MonadGen, Range)
import qualified Hedgehog.Gen as Gen

import Clash.Promoted.Nat
import Clash.Sized.Vector

-- | Generate a potentially empty vector, where each element is produced
-- using the supplied generator. For a non-empty vector, see 'genNonEmptyVec'.
--
genVec :: (MonadGen m, KnownNat n) => m a -> m (Vec n a)
genVec genElem = traverse# id (repeat genElem)

-- | Generate a non-empty vector, where each element is produced using the
-- supplied generator. For a potentially empty vector, see 'genVec'.
--
genNonEmptyVec :: (MonadGen m, KnownNat n, 1 <= n) => m a -> m (Vec n a)
genNonEmptyVec = genVec

data SomeVec atLeast a where
  SomeVec :: SNat n -> Vec (atLeast + n) a -> SomeVec atLeast a

instance (KnownNat atLeast, Show a) => Show (SomeVec atLeast a) where
  show (SomeVec SNat xs) = show xs

genSomeVec
  :: (MonadGen m, KnownNat atLeast)
  => Range Natural
  -> m a
  -> m (SomeVec atLeast a)
genSomeVec rangeElems genElem = do
  numExtra <- Gen.integral rangeElems

  case someNatVal numExtra of
    SomeNat proxy -> SomeVec (snatProxy proxy) <$> genVec genElem
