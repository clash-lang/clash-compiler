{-# LANGUAGE NamedFieldPuns #-}

-- | Nominal type reduction shared by sameDomain and physical clock lookup.
module Clash.GHC.Domain
  ( isEmptyDomain, normalizeDomain, normalizeDomainBounded, isConcreteDomain
  ) where

import Control.Monad.State.Strict (StateT(..), evalStateT)
import Data.Functor.Identity (runIdentity)

import Clash.Core.HasType (inferCoreKindOf)
import Clash.Core.Subst (substTyWith)
import Clash.Core.TyCon (AlgTyConRhs(DataTyCon), TyCon(..), TyConMap)
import Clash.Core.Type
  (Type(..), TypeView(..), TyVar, dataFamilyView, mkTyConApp,
   reduceTypeFamily, tyView)
import Clash.Core.Var (varType)
import qualified Clash.Data.UniqMap as UniqMap

-- A stuck index can become a match after specialization, so it cannot
-- justify falling through to a later closed family equation.
data FamilyMatch a = Matched a | Apart | Stuck

matchDomainEquation :: TyConMap -> [([Type], Type)] -> [Type] -> Maybe Type
matchDomainEquation tcm = choose
 where
  choose [] _ = Nothing
  choose ((lhs,rhs):equations) args
    | length args < length lhs = choose equations args
    | otherwise = case matchTypes tcm (zip lhs args) of
        Matched env ->
          let result = uncurry substTyWith (unzip env) rhs
          in Just (foldl AppTy result (drop (length lhs) args))
        Apart -> choose equations args
        Stuck -> Nothing

-- Match nominal indices. An unresolved family can still acquire a different
-- outer constructor. A repeated variable requires identical arguments.
matchTypes :: TyConMap -> [(Type, Type)] -> FamilyMatch [(TyVar, Type)]
matchTypes tcm = go []
 where
  go env [] = Matched env
  go env ((pat,arg):rest)
    | AnnType _ p <- pat = go env ((p,arg):rest)
    | Just p <- dataFamilyView tcm pat = go env ((p,arg):rest)
    | VarTy v <- pat = case lookup v env of
        Nothing -> go ((v,arg):env) rest
        Just prev | prev == arg -> go env rest
                  | isConcreteDomain tcm prev && isConcreteDomain tcm arg -> Apart
                  | otherwise -> uncertain
    | stuckHead pat || stuckHead arg = uncertain
    | AppTy (VarTy f) p <- pat, AppTy g a <- arg =
        go env ((varType f, inferCoreKindOf tcm g):(VarTy f,g):(p,a):rest)
    | AppTy f p <- pat, AppTy g a <- arg = go env ((f,g):(p,a):rest)
    | pat == arg = go env rest
    | otherwise = Apart
   where
    uncertain = case go env rest of
      Apart -> Apart
      _ -> Stuck

  stuckHead (VarTy _) = True
  stuckHead (tyView -> TyConApp tc args)
    | Just FunTyCon{isDataFamily=False, tyConArity} <- UniqMap.lookup tc tcm =
        length args <= tyConArity
  stuckHead _ = False

-- | A domain is an empty data declaration, possibly indexed. For a data
-- family, inspect the representation of this particular instance.
isEmptyDomain :: TyConMap -> Type -> Bool
isEmptyDomain tcm (tyView -> TyConApp tc args) = case UniqMap.lookup tc tcm of
  Just AlgTyCon{algTcRhs=DataTyCon []} -> True
  Just FunTyCon{isDataFamily=True, tyConSubst} ->
    maybe False (isEmptyDomain tcm) (matchDomainEquation tcm tyConSubst args)
  _ -> False
isEmptyDomain _ _ = False

-- | Nominal domain identity retains newtypes and arrow multiplicities, and
-- observes the source application of a data family rather than its representation.
normalizeDomain :: TyConMap -> Type -> Type
normalizeDomain tcm = runIdentity . normalize (pure ()) tcm

-- | Optional signal metadata must not require an unbounded family reduction.
normalizeDomainBounded :: Int -> TyConMap -> Type -> Maybe Type
normalizeDomainBounded budget tcm t =
  evalStateT (normalize tick tcm t) budget
 where
  tick = StateT $ \remaining ->
    if remaining > 0 then Just ((), remaining - 1) else Nothing

normalize :: Monad m => m () -> TyConMap -> Type -> m Type
normalize tick tcm = go
 where
  go ty = tick >> case tyView ty of
    TyConApp tc args -> do
      args' <- mapM go args
      let app = mkTyConApp tc args'
      case UniqMap.lookup tc tcm of
        Just FunTyCon{isDataFamily=False, tyConArity, tyConSubst}
          | length args' >= tyConArity
          , Just result <- reduce app tyConSubst args' -> go result
        _ | Just source <- dataFamilyView tcm app -> go source
          | otherwise -> pure app
    _ | AppTy f a <- ty -> AppTy <$> go f <*> go a
    OtherType (ForAllTy v t) -> ForAllTy v <$> go t
    OtherType (AnnType _ t) -> go t
    _ -> pure ty

  reduce app [] args
    | all isLiteral args = reduceTypeFamily tcm app
    | otherwise = Nothing
  reduce _ eqs args = matchDomainEquation tcm eqs args

  isLiteral LitTy{} = True
  isLiteral _ = False

-- | A ground type with no unresolved family applications. In particular, two
-- different stuck applications cannot be taken as evidence of inequality.
isConcreteDomain :: TyConMap -> Type -> Bool
isConcreteDomain tcm = go
 where
  go (tyView -> TyConApp tc args) =
    case UniqMap.lookup tc tcm of
      Just FunTyCon{isDataFamily=False} -> False
      _ -> all go args
  go (VarTy _) = False
  go (AppTy f a) = go f && go a
  go (AnnType _ t) = go t
  go (ForAllTy _ _) = False
  go _ = True
