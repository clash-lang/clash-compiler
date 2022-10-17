{-|
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of names.
-}

module Clash.Hedgehog.Core.Name
  ( genKindName
  , genTypeName
  , genTyConName
  , genTermName
  , genDataConName
  , genVarName
  , genFreshName
  , genNames
  ) where

import Control.Monad.Morph (hoist)
import Data.Functor.Identity (Identity(runIdentity))
import qualified Data.Text as Text
import qualified Faker.Lorem as Fake
import Hedgehog (GenT, MonadGen(GenBase, fromGenT))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Gen.Faker as Gen

import Clash.Core.DataCon (DcName)
import Clash.Core.Term (TmName)
import Clash.Core.TyCon (TyConName)
import Clash.Core.Type (KiName, TyName)
import Clash.Core.Name
import Clash.Data.UniqMap (UniqMap)
import qualified Clash.Data.UniqMap as UniqMap

import Clash.Hedgehog.Unique (genUnique)

-- | Generate a name by applying a function to arbitrary text. This is used to
-- ensure that names have the correct case for the thing being named.
--
genOccNameWith :: forall m. MonadGen m => (OccName -> OccName) -> m OccName
genOccNameWith f =
  fromGenT $ hoist @GenT @Identity @(GenBase m)
    (pure . runIdentity)
    (fmap f (Gen.fake Fake.words))

genName :: forall m a. MonadGen m => m OccName -> m (Name a)
genName genOccName =
  Name
    <$> Gen.element [User, System, Internal]
    <*> genOccName
    <*> genUnique
    <*> pure noSrcSpan

genKindName :: forall m. MonadGen m => m KiName
genKindName = genName (genOccNameWith Text.toTitle)

genTypeName :: forall m. MonadGen m => m TyName
genTypeName = genName (genOccNameWith Text.toTitle)

genTyConName :: forall m. MonadGen m => m TyConName
genTyConName = genName (genOccNameWith Text.toTitle)

genTermName :: forall m. MonadGen m => m TmName
genTermName = genName (genOccNameWith Text.toLower)

genDataConName :: forall m. MonadGen m => m DcName
genDataConName = genName (genOccNameWith Text.toTitle)

genVarName :: forall m a. MonadGen m => m (Name a)
genVarName = genName (genOccNameWith Text.toLower)

-- | Generate a name using the given generator, while ensuring the unique of
-- the generated name does not occur in the given @UniqMap@.
--
genFreshName
  :: forall m a b
   . MonadGen m
  => UniqMap b
  -> m (Name a)
  -> m (Name a)
genFreshName used =
  Gen.filterT (not . flip UniqMap.elem used . nameUniq)

mapAccumLM
  :: forall m acc x y
   . Monad m
  => (acc -> x -> m (acc, y))
  -> acc
  -> [x]
  -> m (acc, [y])
mapAccumLM _ acc [] = return (acc, [])
mapAccumLM f acc (x:xs) = do
  (acc', y) <- f acc x
  (acc'', ys) <- mapAccumLM f acc' xs
  return (acc'', y:ys)

-- | Generate a collection of names, from a supplied function to generate names
-- and the number of names to generate.
--
-- TODO While this gives "unique" names because the uniques are different, it
-- can generate multiple names with the same OccName.
genNames
  :: forall m a
   . MonadGen m
  => Int
  -> m (Name a)
  -> m [Name a]
genNames n gen =
  snd <$> mapAccumLM go mempty [1..n]
 where
   go used _ = do
    name <- genFreshName used gen
    pure (UniqMap.insertUnique name used, name)
