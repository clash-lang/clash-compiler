{-|
Copyright   : (C) 2021-2026, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of unique variables and unique containers, and sampling of
things that fit a hole of some type.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

module Clash.Hedgehog.Unique
  ( genUnique
  , genUniqMap
  , sampleUniqMap
  , sampleAnyUniqMap
  , Bias(..)
  , sampleUniqMapBiased
    -- * Sampling from lists
  , sampleList
  , sampleAnyList
  , splitMonoFunTy
  ) where

#if MIN_VERSION_ghc(9,8,4) || (MIN_VERSION_ghc(9,6,7) && !MIN_VERSION_ghc(9,8,0))
#define UNIQUE_IS_WORD64
#endif

import Control.Applicative (Alternative(empty))
import Data.Maybe (mapMaybe)
import Hedgehog (MonadGen, Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.Core.Alpha (aeqTy)
import Clash.Core.Syntax (Type)
import Clash.Core.Type (TypeView(FunTy), isPolyTy, tyView)
import Clash.Data.UniqMap (UniqMap)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Unique

import Clash.Hedgehog.Internal.Bias

genUnique :: forall m. MonadGen m => m Unique
genUnique =
#ifdef UNIQUE_IS_WORD64
  Gen.word64
#else
  Gen.int
#endif
    Range.linearBounded

genUniqMap
  :: forall m k v
   . (MonadGen m, Uniquable k)
  => Range Int
  -> m k
  -> m v
  -> m (UniqMap v)
genUniqMap range genKey genValue =
  UniqMap.fromList <$> Gen.list range ((,) <$> genKey <*> genValue)

-- | Split a type into its argument types and its result type, without going
-- under quantifiers: all of them are in the scope of the type.
splitMonoFunTy :: Type n -> ([Type n], Type n)
splitMonoFunTy ty = case tyView ty of
  FunTy a b -> let (as, r) = splitMonoFunTy b in (a : as, r)
  _ -> ([], ty)

-- | Sample any element which does not have a polymorphic type, giving the
-- types of its arguments.
sampleAnyUniqMap
  :: forall m n v
   . (Alternative m, MonadGen m)
  => (v -> Type n)
  -- ^ The type of an element
  -> UniqMap v
  -> m (v, [Type n])
sampleAnyUniqMap typeOf = sampleAnyList typeOf . UniqMap.elems

-- | See 'sampleAnyUniqMap'
sampleAnyList
  :: forall m n v
   . (Alternative m, MonadGen m)
  => (v -> Type n)
  -> [v]
  -> m (v, [Type n])
sampleAnyList typeOf xs =
  let xs' = filter (not . isPolyTy . typeOf) xs
   in if null xs' then empty else do
     x <- Gen.element xs'
     pure (x, fst (splitMonoFunTy (typeOf x)))

-- | Sample an element which satisfies the predicate, and whose type fits the
-- given hole: either the type is the hole, or it is a function type whose
-- result type fits the hole. Gives the types of the arguments the element
-- needs to be applied to.
sampleUniqMap
  :: forall m n v
   . (Alternative m, MonadGen m)
  => (v -> Type n)
  -- ^ The type of an element
  -> (v -> Bool)
  -> Type n
  -> UniqMap v
  -> m (v, [Type n])
sampleUniqMap typeOf p hole = sampleList typeOf p hole . UniqMap.elems

-- | See 'sampleUniqMap'
sampleList
  :: forall m n v
   . (Alternative m, MonadGen m)
  => (v -> Type n)
  -> (v -> Bool)
  -> Type n
  -> [v]
  -> m (v, [Type n])
sampleList typeOf p hole xs =
  let xs' = mapMaybe (findFit typeOf hole) (filter p xs)
   in if null xs' then empty else Gen.element xs'

sampleUniqMapBiased
  :: forall m n v
   . (Alternative m, MonadGen m, Bias v)
  => (v -> Type n)
  -- ^ The type of an element
  -> (v -> Bool)
  -> Type n
  -> UniqMap v
  -> m (v, [Type n])
sampleUniqMapBiased typeOf p hole xs =
  let xs' = mapMaybe (findFit typeOf hole) (filter p (UniqMap.elems xs))
      bs  = fmap (biasOf . fst) xs'
   in if null xs' then empty else Gen.frequency (zip bs (Gen.constant <$> xs'))

findFit :: (v -> Type n) -> Type n -> v -> Maybe (v, [Type n])
findFit typeOf hole x =
  fmap (x,) (findFitArgs (typeOf x))
 where
  -- NOTE [finding more complex fits]
  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  -- This is not good enough. If I have a hole of type A -> B and I have
  -- a candidate of type forall a. a -> B, I will give up because it is not
  -- alpha equalivalent or a function type. Polymorphic candidates would need
  -- to be instantiated, and that does not fit polymorphic holes either. For
  -- example, if I have the hole A -> B, and the candidate forall a. a -> B,
  -- it would:
  --
  --   1. not aeq to hole, instantiate a
  --   2. not aeq to hole, add a to params
  --   3. not aeq to hole, discard
  --
  -- The correct approach to take here is to figure out which arguments need
  -- to be provided such that the hole and the type of the candidate can be
  -- unified. However, unification is (1) not provided by clash-lib currently
  -- and (2) very non-trivial to implement given we have -XTypeFamilies.
  findFitArgs a
    | aeqTy hole a          = Just []
    | FunTy b c <- tyView a = fmap (b :) (findFitArgs c)
    | otherwise             = Nothing
