{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformations on let-expressions of scoped Clash Core: the ported
  counterparts of "Clash.Normalize.Transformations.Letrec".
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Normalize.Scoped.Transformations.Letrec
  ( deadCode
  , deadCodeWorker
  , removeUnusedBinders
  ) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import GHC.Stack (HasCallStack)

import Clash.Core.Scoped.FreeVars (freeTmVars, teleNameBinders)
import Clash.Core.Scoped.NameMap (nameSetToList)
import Clash.Core.Scoped.Subst
import Clash.Core.Scoped.Syntax
import Clash.Normalize.Types (NormalizeSession)
import Clash.Rewrite.Scoped (TransformContext, changed, tfScope)
import Clash.Rewrite.StrategyDSL
  (Transformation, onLet, toScopedTransformation)

-- | Remove unused let-bindings
deadCode :: Transformation
deadCode = toScopedTransformation "deadCode" (onLet 'deadCodeWorker)

-- | The 'Let' handler of 'deadCode'
deadCodeWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> Bind n l
  -> Term l
  -> NormalizeSession (Term n)
deadCodeWorker ctx e bind body =
  case removeUnusedBinders (tfScope ctx) bind body of
    Just t -> changed t
    Nothing -> return e
{-# SCC deadCodeWorker #-}

-- | Remove the unused bindings of a let-expression. 'Nothing' if all of them
-- are used. See 'Clash.Rewrite.Util.removeUnusedBinders'.
--
-- A binding is used if the body refers to it, or a used binding does. Unlike
-- 'Clash.Rewrite.Util.removeUnusedBinders', the used bindings of a recursive
-- let-binding keep their order, rather than being ordered by unique.
removeUnusedBinders
  :: Distinct n
  => Scope n
  -> Bind n l
  -> Term l
  -> Maybe (Term n)
removeUnusedBinders _ (NonRec b _) body = unsinkIdBinder b body
removeUnusedBinders sc bind@(Rec bs0 _) body0 = case unsinkTerm (teleNameBinders bs0) body0 of
  -- The body refers to none of the binders
  Just body -> Just body
  Nothing ->
    -- Open the binders, to look at their variables
    openBind sc bind body0 $ \_ bind1 body1 -> case bind1 of
      Rec bs xs ->
        let raws = [tmNameRaw n | Var _ _ n <- teleIdVars bs]
            used = usedBinders raws xs body1
        in if IntSet.size used == length raws
             then Nothing
             else Just (keepUsed sc bs xs body1 (map (`IntSet.member` used) raws))
      NonRec {} -> error "removeUnusedBinders: opening a Rec gave a NonRec"

-- | The raw names of the binders of a recursive let-binding that the body
-- refers to, or a used binding does
usedBinders :: [Int] -> [Term l] -> Term l -> IntSet.IntSet
usedBinders raws xs body = go IntSet.empty (rawsOf body)
 where
  rhss = IntMap.fromList (zip raws xs)
  go acc [] = acc
  go acc (r:rs)
    | r `IntSet.member` acc = go acc rs
    | Just rhs <- IntMap.lookup r rhss = go (IntSet.insert r acc) (rawsOf rhs ++ rs)
    | otherwise = go acc rs

-- | The raw names of the free term variables of a term
rawsOf :: Term l -> [Int]
rawsOf = map tmNameRaw . nameSetToList . freeTmVars

-- | Rebuild a recursive let-binding with only the given bindings: fresh
-- binders for those, and a substitution from the old ones to them. The other
-- binders don't occur in the kept right-hand sides and the body, so their
-- entries in the substitution are never used.
keepUsed
  :: (Distinct n, Distinct l)
  => Scope n
  -> Tele IdBinder n l
  -> [Term l]
  -> Term l
  -> [Bool]
  -- ^ Whether to keep the binding, for every binding
  -> Term n
keepUsed sc bs xs body keep =
  withFreshIdBinders sc [(occ, strengthenTyIds bs ty) | (Var occ ty _, True) <- zip vars keep] $
    \sc' bs' vars' -> case vars' of
      [] -> error "keepUsed: no bindings to keep"
      dummy : _ ->
        let substs = fill keep vars'
            fill (True : ks) (v : vs) = v : fill ks vs
            fill (False : ks) vs = dummy : fill ks vs
            fill _ _ = []
        in case extendTmSubstTele (extendSubstScope sc' (idSubst sc)) bs substs of
             Nothing -> error "keepUsed: wrong number of variables"
             Just s ->
               Let (Rec bs' [substTm s x | (x, True) <- zip xs keep]) (substTm s body)
 where
  vars = teleIdVars bs
