{-|
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of core variables.
-}

module Clash.Hedgehog.Core.Var
  ( genTyVar
  , genId
  , genLocalId
  , genGlobalId
  , genVars
  ) where

import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen

import Clash.Core.Name (Name(nameUniq))
import Clash.Core.Term (TmName)
import Clash.Core.Type (Kind, KindOrType, TyName, Type)
import Clash.Core.Var (Id, IdScope(..), TyVar, Var(..))
import qualified Clash.Data.UniqMap as UniqMap

import Clash.Hedgehog.Core.Name (genFreshName)

-- | Generate a fresh type variable of the specified kind.
genTyVar :: forall m. MonadGen m => Kind -> m TyName -> m TyVar
genTyVar kn genName = do
  name <- genName
  pure (TyVar name (nameUniq name) kn)

-- | Generate a fresh identifier of the specified kind.
genId :: forall m. MonadGen m => Type -> m TmName -> m Id
genId ty genName = do
  name  <- genName
  scope <- Gen.element [GlobalId, LocalId]
  pure (Id name (nameUniq name) ty scope)

-- | Generate a fresh local identifier of the specified kind.
genLocalId :: forall m. MonadGen m => Type -> m TmName -> m Id
genLocalId ty =
  fmap setToLocal . genId ty
 where
  setToLocal i@Id{} = i {idScope = LocalId}
  setToLocal i = i

-- | Generate a fresh global identifier of the specified kind.
genGlobalId :: forall m. MonadGen m => Type -> m TmName -> m Id
genGlobalId ty =
  fmap setToGlobal . genId ty
 where
  setToGlobal i@Id{} = i {idScope = LocalId}
  setToGlobal i = i

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

-- | Generate a collection of variables, from a supplied function to generate
-- variables and the kinds / types of variables to generate.
--
-- TODO While this gives "unique" vars because the uniques are different, it
-- can generate multiple vars with the same OccName.
genVars
  :: forall m a
   . MonadGen m
  => (KindOrType -> m (Name a) -> m (Var a))
  -> [KindOrType]
  -> m (Name a)
  -> m [Var a]
genVars genVar kts genName =
  snd <$> mapAccumLM go mempty kts
 where
  go used kt = do
    var <- genVar kt (genFreshName used genName)
    pure (UniqMap.insertUnique var used, var)
