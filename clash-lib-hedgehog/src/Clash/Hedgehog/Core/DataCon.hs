{-|
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random type-directed generation of data constructors.
-}

module Clash.Hedgehog.Core.DataCon
  ( genDataConsFrom
  ) where

import Control.Monad (replicateM, zipWithM)
import Control.Monad.Morph (hoist)
import Data.Either (partitionEithers)
import Data.Functor.Identity (Identity(runIdentity))
import Data.Text (Text)
import qualified Faker.Lorem as Fake
import Hedgehog (GenT, Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Gen.Faker as Gen

import Clash.Core.DataCon
import Clash.Core.Name
import Clash.Core.TyCon
import Clash.Core.Type
import Clash.Core.TysPrim (liftedTypeKind)
import qualified Clash.Data.UniqMap as UniqMap

import Clash.Hedgehog.Core.Monad
import Clash.Hedgehog.Core.Name
import Clash.Hedgehog.Core.Type
import Clash.Hedgehog.Core.Var

-- | Generate a list of data constructors for a type. This biases towards
-- creating constructors which match some common form seen in code, such as
-- simple enums with no fields, or records.
--
genDataConsFrom
  :: forall m
   . (Alternative m, MonadGen m)
  => Range Int
  -- ^ The number of constructors to create for the data type
  -> TyConMap
  -- ^ The types already in scope when defining this type
  -> TyConName
  -- ^ The name of the @AlgTyCon@ the constructors belong to
  -> Kind
  -- ^ The kind of the @AlgTyCon@ the constructors belong to
  -> CoreGenT m [DataCon]
genDataConsFrom range tcm tcn kn = do
  -- We want to bias towards sometimes just having a single constructor. This
  -- is pretty common, e.g. for record types and non-GADT existential types.
  numConstructors <- Gen.choice [Gen.constant 1, Gen.int range]
  names <- genNames numConstructors genDataConName

  -- Universal tyvars are generated now so they can be shared between each
  -- data constructor. This matches what GHC would produce.
  let (knTvs, knTys) = partitionEithers $ fst (splitFunForallTy kn)
  univTvs <- mappend knTvs <$> genVars genTyVar knTys genVarName

  Gen.choice
    [ genSimpleDataCons tcn univTvs names
    , genRecordDataCons tcm tcn univTvs names
    , genAnyDataCons tcm tcn univTvs names
    ]

-- | Generate data constructors for a type
--
--   data D a1 a2 ... an = C1 | C2 | ... | CK
--
-- where every constructor is nullary, but the type constructor may have an
-- arbitrary number of phantom type parameters.
--
genSimpleDataCons
  :: forall m
   . Applicative m
  => TyConName
  -> [TyVar]
  -> [DcName]
  -> CoreGenT m [DataCon]
genSimpleDataCons tcn univTvs =
  pure . zipWith go [1..]
 where
  go :: ConTag -> DcName -> DataCon
  go tag name = MkData
    { dcName = name
    , dcUniq = nameUniq name
    , dcTag = tag
    , dcType = mkTyConApp tcn (fmap VarTy univTvs)
    , dcUnivTyVars = univTvs
    , dcExtTyVars = []
    , dcArgTys = []
    , dcArgStrict = []
    , dcFieldLabels = []
    }

-- | Generate data constructors for a type
--
--   data D a1 a2 ... an
--     = C1 { ... }
--     | C2 { ... }
--     | ...
--     | CK { ... }
--
-- where every constructor is either nullary, or a record.
--
genRecordDataCons
  :: forall m
   . (Alternative m, MonadGen m)
  => TyConMap
  -> TyConName
  -> [TyVar]
  -> [DcName]
  -> CoreGenT m [DataCon]
genRecordDataCons tcm tcn univTvs =
  zipWithM go [1..]
 where
  go :: ConTag -> DcName -> CoreGenT m DataCon
  go tag name = do
    let resTy = mkTyConApp tcn (fmap VarTy univTvs)
    let bound = UniqMap.fromList (zip univTvs univTvs)
    let argGen = genMonoTypeFrom tcm bound liftedTypeKind -- TODO Make polymorphic
    ty <- genWithCodomain resTy argGen

    -- If there are type variables, getMonoTypeFrom is wrong.
    let ([], argTys) = partitionEithers $ fst (splitFunForallTy ty)
    bangs <- traverse (genStrictness tcm) argTys
    fields <- replicateM (length argTys) genFieldLabel

    pure MkData
      { dcName = name
      , dcUniq = nameUniq name
      , dcTag = tag
      , dcType = ty
      , dcUnivTyVars = univTvs
      , dcExtTyVars = []
      , dcArgTys = argTys
      , dcArgStrict = bangs
      , dcFieldLabels = fields
      }

-- | Generate data constructors for a type which does not match any common
-- idiom. Since this can generate any possible data constructor, it can
-- sometimes produce less representative results.
genAnyDataCons
  :: forall m
   . (Alternative m, MonadGen m)
  => TyConMap
  -> TyConName
  -> [TyVar]
  -> [DcName]
  -> CoreGenT m [DataCon]
genAnyDataCons tcm tcn univTvs =
  zipWithM go [1..]
 where
  go :: ConTag -> DcName -> CoreGenT m DataCon
  go tag name = do
    let resTy = mkTyConApp tcn (fmap VarTy univTvs)
    let bound = UniqMap.fromList (zip univTvs univTvs)
    let argGen = genMonoTypeFrom tcm bound liftedTypeKind -- TODO Make polymorphic.
    ty <- genWithCodomain resTy argGen

    -- Determine the argument types from the data constructor type
    -- Generate strictness and field labels from the argument types
    let (extTvs, argTys) = partitionEithers $ fst (splitFunForallTy ty)
    bangs <- traverse (genStrictness tcm) argTys

    pure MkData
      { dcName = name
      , dcUniq = nameUniq name
      , dcTag = tag
      , dcType = ty
      , dcUnivTyVars = univTvs
      , dcExtTyVars = extTvs
      , dcArgTys = argTys
      , dcArgStrict = bangs
      , dcFieldLabels = []
      }

-- TODO genGadt, which can insert ~# arguments after the existential type
-- variables are introduced. I may also want a `genConstraints` in
-- Clash.Hedgehog.Core.Type to generate any constraints for a type.

-- | Generate strictness annotations for data constructor arguments. This
-- ensures that any types which are always strict, e.g. Int#, are strict and
-- types which may be lazy have a random strictness assigned.
--
-- This generator shrinks towards choosing lazy by default for types where it
-- is possible.
genStrictness
  :: forall m. MonadGen m => TyConMap -> Kind -> m DcStrictness
genStrictness tcm kn
  -- Assume that any primitive type constructor is always strict. This may
  -- overapproximate strictness, as it means Type, Nat and Symbol are strict.
  | TyConApp tc [] <- tyView kn
  , Just PrimTyCon{} <- UniqMap.lookup tc tcm
  = pure Strict

  -- Shrink towards laziness as this is the default in Haskell (assuming no
  -- extensions like -XStrict or -XStrictData are enabled).
  | otherwise
  = Gen.element [Lazy, Strict]

-- | Generate a field label for use in a record.
genFieldLabel :: forall m. MonadGen m => m Text
genFieldLabel =
  fromGenT $ hoist @GenT @Identity @(GenBase m)
    (pure . runIdentity)
    (Gen.fake Fake.words)
