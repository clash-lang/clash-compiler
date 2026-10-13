{-|
Copyright   : (C) 2021-2026, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random type-directed generation of data constructors.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Hedgehog.Core.DataCon
  ( genDataConsFrom
  ) where

import Control.Monad (replicateM, zipWithM)
import Control.Monad.Morph (hoist)
import Data.Functor.Identity (Identity(runIdentity))
import Data.Text (Text)
import qualified Faker.Lorem as Fake
import Hedgehog (GenT, Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Gen.Faker as Gen

import Clash.Core.DataCon
import Clash.Core.TyCon
import qualified Clash.Data.UniqMap as UniqMap

import Clash.Core.Subst (openTyBinder)
import Clash.Core.Syntax hiding (Name)
import Clash.Core.Type (TypeView(..), mkTyConApp, tyView)
import Clash.Core.TysPrim (liftedTypeKind)

import Clash.Hedgehog.Core.Monad
import Clash.Hedgehog.Core.Name
import Clash.Hedgehog.Core.Type
import Clash.Hedgehog.Core.Var
import Clash.Hedgehog.Unique (splitMonoFunTy)

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
  -> Kind 'VoidS
  -- ^ The kind of the @AlgTyCon@ the constructors belong to
  -> CoreGenT m [DataCon]
genDataConsFrom range tcm tcn kn = do
  -- We want to bias towards sometimes just having a single constructor. This
  -- is pretty common, e.g. for record types and non-GADT existential types.
  numConstructors <- Gen.choice [Gen.constant 1, Gen.int range]
  names <- genNames numConstructors genDataConName

  -- Universal tyvars are generated now so they can be shared between each
  -- data constructor. This matches what GHC would produce.
  withUnivTyVars emptyEnv kn $ \env univ univVars ->
    Gen.choice
      [ genSimpleDataCons tcn univ univVars names
      , genRecordDataCons tcm tcn env univ univVars names
      , genAnyDataCons tcm tcn env univ univVars names
      ]

-- | Bind the universally quantified type variables of the data constructors
-- of a type constructor with the given kind: the quantified variables of the
-- kind, and a fresh one for every argument of the kind.
withUnivTyVars
  :: forall m n r
   . (MonadGen m, Distinct n)
  => Env n
  -> Kind n
  -> (forall l. DExt n l => Env l -> Tele TyBinder n l -> [Type l] -> m r)
  -> m r
withUnivTyVars env kn k = case kn of
  ForAllTy b kn' ->
    openTyBinder (envScope env) b kn' $ \(sc :: Scope i) b' kn'' ->
      let v = tyBinderVar b'
       in withUnivTyVars (extendEnv sc [v] [] env) kn'' $ \(env2 :: Env l) bs vs ->
            withExtTrans @n @i @l (k env2 (TCons b' bs) (sink v : vs))

  (tyView -> FunTy a kn') ->
    genTyVar env a genVarName $ \(env1 :: Env i) b ->
      withUnivTyVars env1 (sink kn') $ \(env2 :: Env l) bs vs ->
        withExtTrans @n @i @l (k env2 (TCons b bs) (sink (tyBinderVar b) : vs))

  _ -> k env TNil []

-- | The type of a data constructor: its universally quantified type
-- variables, over the given type
forAllTys :: Tele TyBinder n l -> Type l -> Type n
forAllTys TNil ty = ty
forAllTys (TCons b bs) ty = ForAllTy b (forAllTys bs ty)

-- | Generate data constructors for a type
--
--   data D a1 a2 ... an = C1 | C2 | ... | CK
--
-- where every constructor is nullary, but the type constructor may have an
-- arbitrary number of phantom type parameters.
--
genSimpleDataCons
  :: forall m u
   . (Applicative m, DExt 'VoidS u)
  => TyConName
  -> Tele TyBinder 'VoidS u
  -> [Type u]
  -> [DcName]
  -> CoreGenT m [DataCon]
genSimpleDataCons tcn univ univVars =
  pure . zipWith go [1..]
 where
  go :: ConTag -> DcName -> DataCon
  go tag name = mkDataCon
    name
    tag
    (forAllTys univ (mkTyConApp tcn univVars))
    (DataConSig univ TNil [])
    []
    []

-- | Generate data constructors for a type
--
--   data D a1 a2 ... an
--     = C1 { ... }
--     | C2 { ... }
--     | ...
--     | CK { ... }
--
-- where every constructor is a record.
--
genRecordDataCons
  :: forall m u
   . (Alternative m, MonadGen m, DExt 'VoidS u)
  => TyConMap
  -> TyConName
  -> Env u
  -> Tele TyBinder 'VoidS u
  -> [Type u]
  -> [DcName]
  -> CoreGenT m [DataCon]
genRecordDataCons tcm tcn env univ univVars =
  zipWithM go [1..]
 where
  go :: ConTag -> DcName -> CoreGenT m DataCon
  go tag name = do
    let resTy = mkTyConApp tcn univVars
    let argGen = genMonoTypeFrom tcm env liftedTypeKind -- TODO Make polymorphic
    ty <- genWithCodomain env resTy argGen

    let argTys = fst (splitMonoFunTy ty)
    bangs <- traverse (genStrictness tcm) argTys
    fields <- replicateM (length argTys) genFieldLabel

    pure $ mkDataCon
      name
      tag
      (forAllTys univ ty)
      (DataConSig univ TNil argTys)
      bangs
      fields

-- | Generate data constructors for a type which does not match any common
-- idiom. Since this can generate any possible data constructor, it can
-- sometimes produce less representative results.
--
-- TODO The argument types are monomorphic, so these constructors have no
-- existentially quantified type variables yet.
genAnyDataCons
  :: forall m u
   . (Alternative m, MonadGen m, DExt 'VoidS u)
  => TyConMap
  -> TyConName
  -> Env u
  -> Tele TyBinder 'VoidS u
  -> [Type u]
  -> [DcName]
  -> CoreGenT m [DataCon]
genAnyDataCons tcm tcn env univ univVars =
  zipWithM go [1..]
 where
  go :: ConTag -> DcName -> CoreGenT m DataCon
  go tag name = do
    let resTy = mkTyConApp tcn univVars
    let argGen = genMonoTypeFrom tcm env liftedTypeKind -- TODO Make polymorphic.
    ty <- genWithCodomain env resTy argGen

    -- Determine the argument types from the data constructor type
    -- Generate strictness from the argument types
    let argTys = fst (splitMonoFunTy ty)
    bangs <- traverse (genStrictness tcm) argTys

    pure $ mkDataCon
      name
      tag
      (forAllTys univ ty)
      (DataConSig univ TNil argTys)
      bangs
      []

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
  :: forall m n. MonadGen m => TyConMap -> Type n -> m DcStrictness
genStrictness tcm ty
  -- Assume that any primitive type constructor is always strict. This may
  -- overapproximate strictness, as it means Type, Nat and Symbol are strict.
  | TyConApp tc [] <- tyView ty
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
