{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  'viaFoil' runs a scoped transformation as a transformation of Clash Core.
  The normalizer runs on scoped Core, so only the tests use it: to run the
  ports on the regression tests of the transformations of Clash Core, and to
  compare them with those (see "Test.Clash.Differential.Sites"). Phase 7 of
  the port deletes it, together with the transformations of Clash Core.

  = Global bindings

  'viaFoil' first converts the global bindings of Clash Core
  ('Clash.Rewrite.Types.bindings') into those of scoped Core
  ('Clash.Rewrite.Types.scopedBindings'), which the scoped transformations
  read, and afterwards converts the new global bindings back, see
  'withGlobalBindings'.

  = The subterm

  'viaFoil' converts the subterm it is offered into scoped Core, in a scope
  with its free variables, runs the scoped transformation in a context whose
  root has the kinds of the frames of the Clash Core context
  ('Clash.Rewrite.Scoped.Types.embeddedRootContext'), and converts the result
  back if the transformation signaled a change. Otherwise, it returns the
  original subterm, so that unchanged terms keep their sharing.

  The root scope also has the free variables of the let-bindings of the
  context that the evaluator may use for the subterm (see 'rootVars'), so
  that the result of the evaluator is in scope. Those let-bindings are the
  heap of the root of the context, converted into the root scope when the
  evaluator looks them up.

  __Uniques.__ The result of 'viaFoil' gives every binder a unique from the
  unique supply, made unique in the 'Clash.Core.VarEnv.InScopeSet' of the
  context, like 'Clash.Rewrite.Util.mkTmBinderFor' does, see
  "Clash.Core.Scoped.Convert.Fresh". Its free variables are the variables of
  the original subterm and its context.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Clash.Scoped.ViaFoil
  ( viaFoil
  , withGlobalBindings
    -- * Internals, exported for testing
  , rootVars
  ) where

import           Control.Lens                ((%=), (.=))
import qualified Control.Lens                as Lens
import qualified Control.Monad               as Monad
import           Control.Monad.State.Strict  (State, evalState)
import qualified Control.Monad.Writer        as Writer
import qualified Data.IntMap.Strict          as IntMap
import qualified Data.List                   as List
import qualified Data.Monoid                 as Monoid
import           GHC.Stack                   (HasCallStack)

import qualified Clash.Core.FreeVars         as C
import qualified Clash.Core.Term             as C
import qualified Clash.Core.Type             as C
import qualified Clash.Core.Var              as C
import           Clash.Core.VarEnv
  (InScopeSet, VarEnv, eltsVarEnv, elemVarEnv, extendInScopeSetList, lookupVarEnv,
   mkVarEnv)
import qualified Clash.Data.UniqMap          as UniqMap
import           Clash.Driver.Types          (Binding (..), DebugOpts (..))
import           Clash.Rewrite.Types
  (Rewrite, RewriteMonad, bindings, debugOpts, scopedBindings, uniqSupply)
import qualified Clash.Rewrite.Types         as C
import           Clash.Rewrite.Util          (contextLetBindings)
import           Clash.Util                  (curLoc)
import           Clash.Util.Supply           (Supply, splitSupply)

import           Clash.Core.Scoped.Alpha     (aeqTm)
import           Clash.Core.Scoped.Convert
import           Clash.Core.Scoped.Convert.Fresh
  (BackEnv (..), toClashFresh, toClashTermFreshClosed)
import           Clash.Core.Scoped.NameMap
  (NameMap, globalEnvElems, globalEnvFromList, nameMapFromList)
import           Clash.Core.Scoped.Syntax
import           Clash.Rewrite.Scoped.Types

-- * viaFoil

-- | Run a scoped transformation as a transformation of Clash Core, see the
-- module header. Uninstrumented: wrap it in 'Clash.Rewrite.Util.apply' to
-- count it and check its result.
viaFoil
  :: HasCallStack
  => String
  -- ^ Name of the transformation, for error messages
  -> ScopedRewrite extra
  -> Rewrite extra
viaFoil name (ScopedTransform rw) ctx e = withGlobalBindings $
  withConvEnv tvs ids $ \env ->
    let tvsS = [(tv, fromClashType env (C.VarTy tv)) | tv <- tvs]
        idsS = [(i, fromClashTerm env (C.Var i)) | i <- ids]
        frames = map frameKind (C.tfContext ctx)
        root = embeddedRootContext (convScope env) frames
                 (rootHeap env idsS (C.tfContext ctx) e)
        e0 = fromClashTerm env e
    in do
      (e1, Monoid.getAny -> changed) <- Writer.listen (rw root e0)
      if changed
        then withSupply (toClashFresh (rootInScope tvs ids) (backEnv tvsS idsS) e1)
        else do
          opts <- Lens.view debugOpts
          Monad.when (dbg_invariants opts && not (aeqTm e0 e1)) $
            error ($(curLoc) ++ "Expression changed without notice ("
                   ++ name ++ "), under viaFoil")
          pure e
 where
  (tvs, ids) = rootVars (C.tfContext ctx) e

  rootInScope :: [C.TyVar] -> [C.Id] -> InScopeSet
  rootInScope tvs' ids' =
    extendInScopeSetList (extendInScopeSetList (C.tfInScope ctx) tvs') ids'

-- | Run a computation of scoped Core with the global bindings of Clash Core
-- ('Clash.Rewrite.Types.bindings'): convert them into those of scoped Core
-- ('Clash.Rewrite.Types.scopedBindings') before, and convert the global
-- bindings that it adds back after.
withGlobalBindings :: HasCallStack => RewriteMonad extra a -> RewriteMonad extra a
withGlobalBindings m = do
  bm <- Lens.use bindings
  scopedBindings %= \sb ->
    globalEnvFromList
      [ (bindingId b, fromClashTermClosed <$> b) | b <- UniqMap.elems bm ]
      <> sb
  a <- m
  sb <- Lens.use scopedBindings
  new <- withSupply $ sequence
    [ (\tm -> (globalIdLegacy (bindingId b), b {bindingTerm = tm})) <$> toClashTermFreshClosed (bindingTerm b)
    | b <- globalEnvElems sb
    , not (globalIdLegacy (bindingId b) `UniqMap.elem` bm) ]
  bindings %= UniqMap.insertMany new
  pure a

-- | The kind of a frame of Clash Core
frameKind :: C.CoreContext -> FrameKind
frameKind = \case
  C.AppFun -> KAppFun
  C.AppArg a -> KAppArg a
  C.TyAppC -> KTyAppC
  C.LetBinding b _ -> KLetBinding (C.varName b)
  C.LetBody _ -> KLetBody
  C.LamBody _ -> KLamBody
  C.TyLamBody _ -> KTyLamBody
  C.CaseAlt _ -> KCaseAlt
  C.CaseScrut -> KCaseScrut
  C.CastBody -> KCastBody
  C.TickC _ -> KTickC

-- | The variables of the root scope of 'viaFoil': the free variables of the
-- subterm, and those of the right-hand sides of the let-bindings of its
-- context ('Clash.Core.Term.LetBody' frames) that the evaluator may use for
-- it, transitively. These are the bindings of variables in the set so far.
-- Type variables are ordered such that their kinds only mention the ones
-- before them.
--
-- This overapproximates the bindings the evaluator uses: e.g. a binding of a
-- variable shadowed between it and the subterm is unusable (see
-- 'Clash.Rewrite.Util.whnfHeap'). The variables of its right-hand side are
-- then added needlessly. That is harmless, as long as a variable of the
-- subterm wins from one of the context with the same unique, which is what
-- the 'Clash.Core.VarEnv.VarEnv's do: the subterm comes first.
rootVars :: C.Context -> C.Term -> ([C.TyVar], [C.Id])
rootVars frames e =
  (C.dependencySortTyVars (eltsVarEnv tvsN), eltsVarEnv idsN)
 where
  (tvs0, ids0) = freeVarsOf e
  (tvsN, idsN) = List.foldl' frame (tvs0, ids0) frames

  frame acc@(_, ids) = \case
    C.LetBody bs | any ((`elemVarEnv` ids) . fst) bs -> close bs acc
    _ -> acc

  -- Add the free variables of the right-hand sides of the needed bindings,
  -- which may make more bindings of the same frame needed
  close bs acc0@(_, ids0') = go acc0 (filter (`elemVarEnv` bsEnv) (eltsVarEnv ids0'))
   where
    bsEnv = mkVarEnv [(x, rhs) | (x, rhs) <- bs]
    -- Every variable enters the work list at most once: when it is added to
    -- the set.
    go acc [] = acc
    go acc@(tvs, ids) (x:xs) = case lookupVarEnv x bsEnv of
      Nothing -> go acc xs
      Just rhs ->
        let (tvsR, idsR) = freeVarsOf rhs
            new = [i | i <- eltsVarEnv idsR, not (i `elemVarEnv` ids)]
        in go (tvs <> tvsR, ids <> idsR) (filter (`elemVarEnv` bsEnv) new ++ xs)

  freeVarsOf :: C.Term -> (VarEnv C.TyVar, VarEnv C.Id)
  freeVarsOf t =
    ( mkVarEnv [(tv, tv) | tv <- Lens.toListOf C.termFreeTyVars t]
    , mkVarEnv [(i, i) | i <- Lens.toListOf C.freeLocalIds t] )

-- | The let-bindings of the context of the root that are valid at the
-- subterm (see 'Clash.Rewrite.Util.contextLetBindings'), of the variables of
-- the root scope. A binding is converted into the root scope when it is
-- looked up: 'fmap' of a 'NameMap' is lazy.
rootHeap
  :: Distinct n
  => ConvEnv n
  -> [(C.Id, Term n)]
  -> C.Context
  -> C.Term
  -> NameMap TmName n (Term n)
rootHeap env ids frames e =
  fmap (\(Lazy t) -> t) $ nameMapFromList
    [ (x, Lazy (fromClashTerm env rhs))
    | (u, rhs) <- UniqMap.toList (contextLetBindings frames e)
    , Just x <- [IntMap.lookup (fromIntegral u) names] ]
 where
  names = IntMap.fromList [ (fromIntegral (C.varUniq i), x) | (i, Var _ _ x) <- ids ]

-- | A box, so that a strict map doesn't evaluate its contents
data Lazy a = Lazy a

-- | The Clash variables of the root scope of 'viaFoil'
backEnv :: [(C.TyVar, Type n)] -> [(C.Id, Term n)] -> BackEnv
backEnv tvs ids = BackEnv
  (IntMap.fromList [(tyRaw ty, tv) | (tv, ty) <- tvs])
  (IntMap.fromList [(tmRaw tm, i) | (i, tm) <- ids])
 where
  tyRaw = \case
    VarTy _ _ n -> tyNameRaw n
    _ -> error "backEnv: not a type variable"
  tmRaw = \case
    Var _ _ n -> tmNameRaw n
    _ -> error "backEnv: not a variable"

-- | Run a computation with a split of the unique supply
withSupply :: State Supply a -> RewriteMonad extra a
withSupply m = do
  sup <- Lens.use uniqSupply
  let (sup1, sup2) = splitSupply sup
  uniqSupply .= sup2
  pure (evalState m sup1)
