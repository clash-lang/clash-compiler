{-|
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of unique variables and unique containers.
-}

{-# LANGUAGE TupleSections #-}

module Clash.Hedgehog.Unique
  ( genUnique
  , genUniqMap
  , sampleUniqMap
  , sampleAnyUniqMap
  , Bias(..)
  , sampleUniqMapBiased
  ) where

import Control.Applicative (Alternative(empty))
import Data.Either (rights)
import Hedgehog (MonadGen, Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.Core.HasType
import Clash.Core.Subst (aeqType)
import Clash.Core.Type
import Clash.Data.UniqMap (UniqMap)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Unique

import Clash.Hedgehog.Internal.Bias

genUnique :: forall m. MonadGen m => m Unique
genUnique = Gen.int Range.linearBounded

genUniqMap
  :: forall m k v
   . (MonadGen m, Uniquable k)
  => Range Int
  -> m k
  -> m v
  -> m (UniqMap v)
genUniqMap range genKey genValue =
  UniqMap.fromList <$> Gen.list range ((,) <$> genKey <*> genValue)

sampleAnyUniqMap
  :: forall m v
   . (Alternative m, MonadGen m, HasType v)
  => UniqMap v
  -> m (v, [Type])
sampleAnyUniqMap xs =
  let xs' = UniqMap.filter (not . isPolyTy . coreTypeOf) xs
   in if UniqMap.null xs' then empty else do
     x <- Gen.element (UniqMap.elems xs')
     let holes = rights . fst $ splitFunForallTy (coreTypeOf x)

     pure (x, holes)

sampleUniqMap
  :: forall m v
   . (Alternative m, MonadGen m, HasType v)
  => (v -> Bool)
  -> Type
  -> UniqMap v
  -> m (v, [Type])
sampleUniqMap p hole xs =
  let xs' = UniqMap.mapMaybe findFit (UniqMap.filter p xs)
   in if UniqMap.null xs' then empty else Gen.element (UniqMap.elems xs')
 where
  findFit x =
    fmap (x,) (findFitArgs (coreTypeOf x))

  -- NOTE [finding more complex fits]
  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  -- This is not good enough. If I have a hole of type A -> B and I have
  -- a candidate of type forall a. a -> B, I will give up because it is not
  -- alpha equalivalent or a function type. I could return [Either TyVar Type]
  -- and include foralls, but this would still not fit polymorphic holes. For
  -- example, if I have the hole A -> B, and the candidate forall a. a -> B, it
  -- would:
  --
  --   1. not aeq to hole, add Left a to params
  --   2. not aeq to hole, add Right a to params
  --   3. not aeq to hole, discard
  --
  -- The correct approach to take here is to figure out which arguments need
  -- to be provided such that the hole and the type of the candidate can be
  -- unified. However, unification is (1) not provided by clash-lib currently
  -- and (2) very non-trivial to implement given we have -XTypeFamilies.
  findFitArgs a
    | aeqType hole a        = Just []
    | FunTy b c <- tyView a = fmap (b :) (findFitArgs c)
    | otherwise             = Nothing

sampleUniqMapBiased
  :: forall m v
   . (Alternative m, MonadGen m, HasType v, Bias v)
  => (v -> Bool)
  -> Type
  -> UniqMap v
  -> m (v, [Type])
sampleUniqMapBiased p hole xs =
  let xs' = UniqMap.elems $ UniqMap.mapMaybe findFit (UniqMap.filter p xs)
      bs  = fmap (biasOf . fst) xs'
   in if null xs' then empty else Gen.frequency (zip bs (Gen.constant <$> xs'))
  where
  findFit x =
    fmap (x,) (findFitArgs (coreTypeOf x))

  findFitArgs a
    | aeqType hole a        = Just []
    | FunTy b c <- tyView a = fmap (b :) (findFitArgs c)
    | otherwise             = Nothing
