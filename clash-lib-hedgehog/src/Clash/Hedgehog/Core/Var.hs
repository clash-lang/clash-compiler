{-|
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of core variables.
-}

module Clash.Hedgehog.Core.Var
  ( genAttr'
  , genTyVar
  , genId
  , genLocalId
  , genGlobalId
  , genVars
  ) where

import Hedgehog (MonadGen, Range)
import qualified Hedgehog.Gen as Gen

import Clash.Core.Name (Name(nameUniq))
import Clash.Core.Term (TmName)
import Clash.Core.Type (Kind, KindOrType, TyName, Type)
import Clash.Core.Var (Attr'(..), Id, IdScope(..), TyVar, Var(..))
import qualified Clash.Data.UniqMap as UniqMap

import Clash.Hedgehog.Core.Name (genFreshName)

genAttr' :: forall m. MonadGen m => Range Int -> m Attr'
genAttr' range =
  Gen.choice
    [ BoolAttr' <$> genAlphaNum <*> Gen.bool
    , IntegerAttr' <$> genAlphaNum <*> genInteger
    , StringAttr' <$> genAlphaNum <*> genAlphaNum
    , Attr' <$> genAlphaNum
    ]
 where
  genAlphaNum = Gen.string range Gen.alphaNum
  genInteger  = toInteger <$> Gen.integral range

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
  fmap (\i -> i { idScope = LocalId }) . genId ty

-- | Generate a fresh global identifier of the specified kind.
genGlobalId :: forall m. MonadGen m => Type -> m TmName -> m Id
genGlobalId ty =
  fmap (\i -> i { idScope = GlobalId }) . genId ty

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
