{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2022, Google Inc.,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformations for inlining on scoped Clash Core.

  = Globals

  Only global variables ('GlobalVar') are inlined, looked up in the global
  bindings ('Clash.Rewrite.Scoped.Globals.lookupBinding'), counted in the
  inline history, and taken to be no-ops by 'collapseRHSNoops'. A local
  variable with the unique of a global binder is a 'Var', which never matches
  (#405). The body of a global binding is a closed term, which is moved into
  the scope of the inlined occurrence for free ('sinkClosed'). Its binders get
  fresh names when a traversal goes under them.

  The function being normalized ('Clash.Rewrite.Types.curFun') is a global,
  which need not have a binding: e.g. the fake global of
  'Clash.Normalize.Scoped.Util.normalizeWorkFreeApp' doesn't.

  = Let-bindings

  The transformations on let-expressions work on its bindings in the scope of
  the body ('Clash.Rewrite.Scoped.Util.openLetBindings'), so a right-hand side
  of a non-recursive binding can't refer to its own binder.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Normalize.Scoped.Transformations.Inline
  ( bindConstantVar
  , bindConstantVarWorker
  , inlineBndrsCleanup
  , inlineCast
  , inlineCastWorker
  , inlineCleanup
  , inlineCleanupWorker
  , collapseRHSNoops
  , collapseRHSNoopsWorker
  , inlineNonRep
  , inlineNonRepWorker
  , inlineOrLiftNonRep
  , inlineOrLiftNonRepWorker
  , inlineSimIO
  , inlineSimIOWorker
  , inlineSmall
  , inlineSmallWorker
  , inlineWorkFree
  , inlineWorkFreeWorker
  ) where

import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import Control.Monad ((>=>))
import Control.Monad.Extra (anyM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Control.Monad.Writer as Writer
import Data.Default (Default (..))
import Data.Either (lefts, rights)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import qualified Data.Text as Text
import qualified Data.Text.Extra as Text
import GHC.BasicTypes.Extra (isNoInline)
import GHC.Stack (HasCallStack)

import Clash.Core.Basic (WorkInfo (..))
import qualified Clash.Explicit.SimIO as SimIO
import qualified Clash.Sized.Internal.BitVector as BV (Bit (Bit), BitVector (BV), xToBV)

import Clash.Annotations.Primitive (extractPrim)
import Clash.Core.DataCon (DataCon (..))
import qualified Clash.Core.Name as C
import Clash.Core.Pretty (PrettyOptions (..), showPpr, showPpr')
import Clash.Debug (trace)
import Clash.Driver.Types (Binding (..))
import Clash.Primitives.Types (CompiledPrimMap, Primitive (..), TemplateKind (..))
import Clash.Rewrite.StrategyDSL
  ( Transformation, onAppNode, onLet, onTickNode, onTyAppNode, onVarNode
  , toScopedTransformation )
import Clash.Rewrite.Types
  ( curFun, inlineConstantLimit, inlineFunctionLimit, inlineLimit
  , inlineWFCacheLimit, primitives, typeEnv )
import Clash.Normalize.Types (NormalizeSession)
import Clash.Util (curLoc)
import qualified Clash.Util.Interpolate as I

import Clash.Core.Scoped.FreeVars
  (freeTmVarOccurrences, freeTmVars, hasFreeVarsType, tmNameOccursIn)
import Clash.Core.Scoped.HasType (inferCoreTypeOf)
import Clash.Core.Scoped.NameMap
import Clash.Core.Scoped.Pretty ()
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term
  (collectArgs, collectArgsTicks, collectTicks, mkApps, mkTicks, stripTicks)
import Clash.Core.Scoped.TermInfo (isClosed, isLocalVar, termSizeSmallerThan)
import Clash.Core.Scoped.Type
  (TypeView (..), isClassTy, isPolyFunCoreTy, isSignalType, tyView)
import Clash.Core.Scoped.Util (primUCo)
import Clash.Normalize.Scoped.Util
  ( addNewInline, alreadyInlined, isRecursiveBndr, mkInlineTick
  , normalizeTopLvlBndr, normalizeWorkFreeApp )
import Clash.Rewrite.Scoped
  ( Frame (CaseScrut), FrameKind (..), ScopedTransform (..), TransformContext, allR
  , changed, ctxFrames, pushFrame, tfScope, underAlt )
import Clash.Rewrite.Scoped.Globals (isTopEntity, lookupBinding)
import Clash.Rewrite.Scoped.Util
  ( LetBinding (..), inlineBinders, inlineOrLiftBinders, isJoinPointIn
  , isUntranslatable, isUntranslatableType, isVoidWrapper, keepBindings
  , openLetBindings, substVars, zoomExtra )
import Clash.Rewrite.Scoped.WorkFree (isWorkFreeIsh)

-- | The function being normalized, see 'Clash.Rewrite.Types.curFun'. It is
-- always a global, but need not have a binding.
curFunGlobal :: NormalizeSession GlobalId
curFunGlobal = fst <$> Lens.use curFun

-- | Inline let-bindings when the RHS is either a local variable reference or
-- is constant (except clock or reset generators)
bindConstantVar :: Transformation
bindConstantVar =
  toScopedTransformation "bindConstantVar" (onLet 'bindConstantVarWorker)

-- | The 'Let' handler of 'bindConstantVar'
bindConstantVarWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n -> Term n -> Bind n l -> Term l -> NormalizeSession (Term n)
bindConstantVarWorker = inlineBinders test
 where
  test :: Distinct o => Scope o -> LetBinding o -> NormalizeSession Bool
  test sc (LetBinding {lbName, lbRhs = stripTicks -> e}) = case isLocalVar e of
    -- Don't inline `let x = x in x`, it throws  us in an infinite loop
    True -> return (not (lbName `tmNameOccursIn` e))
    _ -> do
      cf <- curFunGlobal
      -- Don't inline globally recursive calls, it prevents the recToLetRec
      -- transformation from transforming global recursion to local recursion.
      -- See https://github.com/clash-lang/clash-compiler/issues/2839
      case e of
        GlobalVar g | g == cf -> return False
        _ -> do
          te <- Lens.view typeEnv
          -- Don't inline things that perform work, it increases the circuit
          -- size.
          case isWorkFreeIsh te sc e of
            True -> Lens.view inlineConstantLimit >>= \case
              0 -> return True
              n -> return (termSizeSmallerThan (n + 1) e)
            _ -> return False
{-# SCC bindConstantVarWorker #-}

-- | Only inline casts that just contain a variable, because these are
-- guaranteed work-free. These are the result of the 'splitCastWork'
-- transformation.
inlineCast :: Transformation
inlineCast = toScopedTransformation "inlineCast" (onLet 'inlineCastWorker)

-- | The 'Let' handler of 'inlineCast'
inlineCastWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n -> Term n -> Bind n l -> Term l -> NormalizeSession (Term n)
inlineCastWorker = inlineBinders test
 where
  test :: Scope o -> LetBinding o -> NormalizeSession Bool
  test _ lb = return $ case lbRhs lb of
    Cast (stripTicks -> Var {}) _ _ -> True
    Cast (stripTicks -> GlobalVar {}) _ _ -> True
    _ -> False
{-# SCC inlineCastWorker #-}

-- | Inline anything of type `SimIO`: IO actions cannot be shared
inlineSimIO :: Transformation
inlineSimIO = toScopedTransformation "bindSimIO" (onLet 'inlineSimIOWorker)

-- | The 'Let' handler of 'inlineSimIO'
inlineSimIOWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n -> Term n -> Bind n l -> Term l -> NormalizeSession (Term n)
inlineSimIOWorker = inlineBinders test
 where
  test :: Scope o -> LetBinding o -> NormalizeSession Bool
  test _ lb = case tyView (lbType lb) of
    TyConApp tc _ -> return $! C.nameOcc tc == Text.showt ''SimIO.SimIO
    _ -> return False
{-# SCC inlineSimIOWorker #-}

-- | Inline or lift let-bindings of a non-representable type, see
-- 'inlineOrLiftBinders'
inlineOrLiftNonRep :: Transformation
inlineOrLiftNonRep =
  toScopedTransformation "bindOrLiftNonRep" (onLet 'inlineOrLiftNonRepWorker)

-- | The 'Let' handler of 'inlineOrLiftNonRep'
inlineOrLiftNonRepWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n -> Term n -> Bind n l -> Term l -> NormalizeSession (Term n)
inlineOrLiftNonRepWorker = inlineOrLiftBinders nonRepTest inlineTest
 where
  nonRepTest :: Distinct o => Scope o -> LetBinding o -> NormalizeSession Bool
  nonRepTest sc lb = isUntranslatableType False sc (lbType lb)

  inlineTest :: [LetBinding o] -> Term o -> LetBinding o -> Bool
  inlineTest lbs body = \lb ->
    -- We do __NOT__ inline:
    not $ or
      [ -- 1. recursive let-binders
        -- (already checked in inlineOrLiftBinders)
        -- 2. join points (which are not void-wrappers)
        isJoinPointIn (lbName lb) lbs body && not (isVoidWrapper (lbRhs lb))
        -- 3. binders that are used more than once in the body, because
        --    it makes CSE a whole lot more difficult.
        --
        -- XXX: Check whether we can extend this to the binders as well
      , maybe False (> 1) (lookupNameMap (lbName lb) bodyFreeOccs)
      ]
   where
    bodyFreeOccs = freeTmVarOccurrences body
{-# SCC inlineOrLiftNonRepWorker #-}

-- * inlineCleanup

-- | The occurrences of the free term variables of a term, see
-- 'freeTmVarOccurrences'
type Occs l = NameMap TmName l Int

-- | Mark to track progress of 'reduceBindersCleanup'
data Mark
  = MTemp
  -- ^ Will eventually form a recursive cycle
  | MDone
  -- ^ Processed, non-recursive
  | MRec
  -- ^ Processed, recursive

-- | Used (transitively) by 'inlineCleanup' to inline to-inline let-binders
-- into the other to-inline let-binders.
--
-- The let-bindings are those of a let-expression opened with
-- 'openLetBindings', in the scope of its body. A free variable of a
-- right-hand side that is not one of its binders is a variable of the scope of
-- the let-expression: there's nothing to do for it.
reduceBindersCleanup
  :: (HasCallStack, Distinct l)
  => Scope l
  -> NameMap TmName l (LetBinding l, Occs l)
  -- ^ Original let-binders with their free variables (+ #occurrences)
  -> (NameMap TmName l (Term l), Occs l, NameMap TmName l (LetBinding l, Occs l, Mark))
  -- ^ Accumulated:
  --
  -- 1. The substitution built up so far
  -- 2. The free variables of the range of the substitution
  -- 3. Processed let-binders with their free variables and a tag to mark
  --    the progress, see 'Mark'
  -> TmName l
  -- ^ The let-binding that we want to simplify
  -> (NameMap TmName l (Term l), Occs l, NameMap TmName l (LetBinding l, Occs l, Mark))
  -- ^ Same as the third argument
reduceBindersCleanup scL origInl (!subst, !substFVs, !doneInl) u =
  case lookupNameMap u doneInl of
    Nothing -> case lookupNameMap u origInl of
      -- Not a let-binding we want to inline: cannot extend the substitution
      Nothing -> (subst, substFVs, doneInl)
      Just (lb, eFVs) ->
        -- Simplify the transitive dependencies
        let (sM, substFVsE, doneInl1) =
              List.foldl'
                (reduceBindersCleanup scL origInl)
                ( emptyNameMap
                -- It's okay/needed to over-approximate the free variables of
                -- the range of the new substitution by including the free
                -- variables of the original let-binder, because this set of
                -- free variables is only used to check whether let-binding will
                -- become self-recursive after applying the substitution.
                --
                -- That is, it was already self-recursive, or becomes
                -- self-recursive after applying the substitution because it was
                -- part of a recursive group. And we do not want to inline
                -- recursive binders.
                , eFVs
                -- Temporarily extend the processing environment with the
                -- let-binding so we don't end up in a loop in case there is a
                -- recursive group.
                , insertNameMap u (lb, eFVs, MTemp) doneInl )
                (nameMapKeys eFVs)

            e1 = substVars scL sM (lbRhs lb)

            -- The let-binding is part of a cycle if it refers to a let-binding
            -- that is still being processed: itself, or one of the let-bindings
            -- that led us here. We must not inline it then: its substituted
            -- term would refer to that let-binding, which we might inline and
            -- drop later on.
            inCycle = any isTemp (nameMapKeys substFVsE)
            isTemp i = case lookupNameMap i doneInl1 of
              Just (_, _, MTemp) -> True
              _ -> False
        in  if inCycle then
              -- We cannot inline recursive let-bindings, so we do not extend
              -- the substitution.
              ( subst
              , substFVs
              -- And we explicitly mark the let-binding as recursive in the
              -- processing environment. So that it will be kept around at the
              -- end of 'inlineCleanup'
              , insertNameMap u (lb {lbRhs = e1}, substFVsE, MRec) doneInl1
              )
            else
              -- Extend the substitution
              ( insertNameMap u e1 subst
              , unionWithNameMap (+) substFVsE substFVs
              -- Mark the let-binding a fully "reduced", so we don't repeat
              -- this process when we encounter it again.
              , insertNameMap u (lb {lbRhs = e1}, substFVsE, MDone) doneInl1
              )
    -- It's already been processed, just extend the substitution
    Just (lb, eFVs, MDone) ->
      ( insertNameMap u (lbRhs lb) subst
      , unionWithNameMap (+) eFVs substFVs
      , doneInl
      )
    -- It's either recursive (MRec), or part of a recursive group (MTemp)
    -- where we originally entered a different part of the cycle. Regardless,
    -- we do not extend the substitution.
    Just _ -> (subst, substFVs, doneInl)
{-# SCC reduceBindersCleanup #-}

-- | Used by 'inlineCleanup' to inline binders that we want to inline into the
-- binders that we want to keep.
--
-- Gives the bindings to keep: the given ones, followed by those of the
-- bindings to inline that turned out to be recursive, in the order of their
-- names.
inlineBndrsCleanup
  :: (HasCallStack, Distinct l)
  => Scope l
  -> NameMap TmName l (LetBinding l, Occs l)
  -- ^ Original let-binders with their free variables (+ #occurrences), that we
  -- want to inline
  -> NameMap TmName l (LetBinding l, Occs l, Mark)
  -- ^ Processed let-binders with their free variables and a tag to mark the
  -- progress, see 'Mark'
  -> [(LetBinding l, Occs l)]
  -- ^ The let-binders with their free variables (+ #occurrences), that we want
  -- to keep
  -> [LetBinding l]
inlineBndrsCleanup scL origInl = go
 where
  go doneInl [] =
    -- If some of the let-binders that we wanted to inline turn out to be
    -- recursive, then we have to keep those around as well, as we weren't able
    -- to inline them. Furthermore, for every recursive binder there might still
    -- be non-inlined variables left, see #1337.
    [ lb {lbRhs = substVars scL substM (lbRhs lb)}
    | (lb, eFVs, MRec) <- nameMapElems doneInl
    , let (substM, _, _) =
            List.foldl'
              (reduceBindersCleanup scL emptyNameMap)
              (emptyNameMap, emptyNameMap, doneInl)
              (nameMapKeys eFVs)
    ]
  go !doneInl0 ((lb, eFVs) : il) =
    let (sM, _, doneInl1) =
          List.foldl'
            (reduceBindersCleanup scL origInl)
            (emptyNameMap, emptyNameMap, doneInl0)
            (nameMapKeys eFVs)
    in  lb {lbRhs = substVars scL sM (lbRhs lb)} : go doneInl1 il
{-# SCC inlineBndrsCleanup #-}

-- | Given a function in the desired normal form, inline all the following
-- let-bindings:
--
-- Let-bindings with an internal name that is only used once, where it binds:
--   * a primitive that will be translated to an HDL expression (as opposed to
--     a HDL declaration)
--   * a projection case-expression (1 alternative)
--   * a data constructor
--   * I/O actions
inlineCleanup :: Transformation
inlineCleanup = toScopedTransformation "inlineCleanup" (onLet 'inlineCleanupWorker)

-- | The 'Let' handler of 'inlineCleanup'
inlineCleanupWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n -> Term n -> Bind n l -> Term l -> NormalizeSession (Term n)
inlineCleanupWorker ctx node bind body0 = do
  prims <- Lens.view primitives
  openLetBindings sc bind body0 $ \scL bs lbs body ->
    -- For all let-bindings, count the number of times they are referenced.
    -- We only inline let-bindings which are referenced only once, otherwise
    -- we would lose sharing.
    let bindsFvs = [(lb, freeTmVarOccurrences (lbRhs lb)) | lb <- lbs]
        allOccs = List.foldl' (unionWithNameMap (+)) emptyNameMap (map snd bindsFvs)
        bodyFVs = freeTmVars body
        (il, keep) = List.partition (isInteresting allOccs prims bodyFVs) bindsFvs
        keep' = inlineBndrsCleanup scL
                  (nameMapFromList [(lbName lb, x) | x@(lb, _) <- il])
                  emptyNameMap
                  keep
    in  if null il then
          return node
        else case keepBindings sc bs keep' body of
          Just e -> changed e
          Nothing -> error "inlineCleanup: an inlined binder remains"
 where
  sc = tfScope ctx

  -- Determine whether a let-binding is interesting to inline
  isInteresting
    :: Occs l'
    -> CompiledPrimMap
    -> NameSet TmName l'
    -> (LetBinding l', Occs l')
    -> Bool
  isInteresting allOccs prims bodyFVs (lb, _)
    | lbName lb `memberNameSet` bodyFVs = False
    -- Try to keep user defined names, but inline names generated by GHC or
    -- Clash. For example, if a user were to write:
    --
    --    x = 2 * y
    --
    -- Even if 'x' is only used once, we'd like to keep it around to produce
    -- more readable HDL. In contrast, if a user were to write:
    --
    --    let x = f (2 * y)
    --
    -- ANF would transform that to:
    --
    --    let x = f f_arg; f_arg = 2 * y
    --
    -- In that case, there's no harm in inlining f_arg.
    | C.nameSort (lbOcc lb) /= C.User
    = case tm of
        Prim pInfo
          | let nm = primName pInfo
          , Just (extractPrim -> Just p@(BlackBox {})) <- HashMap.lookup nm prims
          , TExpr <- kind p
          , Just occ <- lookupNameMap (lbName lb) allOccs
          , occ < 2
          -> True
          | otherwise
          -> primName pInfo `elem` ["Clash.Explicit.SimIO.bindSimIO#"]
        Case _ _ [_] -> True
        Data _ -> True
        Case _ aTy (_:_:_)
          | TyConApp nm _ <- tyView aTy
          , C.nameOcc nm == Text.showt ''SimIO.SimIO
          -> True
        _ -> False
    | otherwise
    = case tm of
        Prim pInfo
          | primName pInfo `elem`
                      [ Text.showt 'SimIO.openFile
                      , Text.showt 'SimIO.getChar
                      , Text.showt 'SimIO.isEOF
                      ]
          , Just occ <- lookupNameMap (lbName lb) allOccs
          , occ < 2
          -> True
          | otherwise
          -> primName pInfo `elem` ["Clash.Explicit.SimIO.bindSimIO#"]
        Case _ _ [Alt (DataPat dcE _ _) _]
          -> let nm = C.nameOcc (dcName dcE)
             in -- Inlines WW projection that exposes internals of the BitVector types
                nm == Text.showt 'BV.BV  ||
                nm == Text.showt 'BV.Bit ||
                -- Inlines projections out of constraint-tuples (e.g. HiddenClockReset).
                -- The 'GHC.Classes' module hosts these on GHC <= 9.12; on
                -- GHC >= 9.14 'base'\''s GHC.Classes re-exports them from
                -- 'ghc-internal:GHC.Internal.Classes'.
                "GHC.Classes" `Text.isPrefixOf` nm ||
                "GHC.Internal.Classes" `Text.isPrefixOf` nm
        Case _ aTy (_:_:_)
          | TyConApp nm _ <- tyView aTy
          , C.nameOcc nm == Text.showt ''SimIO.SimIO
          -> True
        _ -> False
   where
    tm = fst (collectArgs (lbRhs lb))
{-# SCC inlineCleanupWorker #-}

-- * collapseRHSNoops

{- Note [relation collapseRHSNoops and inlineCleanup]
The `collapseRHSNoops` transformation replaces functions/primitives that are
the identity in HDL, but not in Haskell, by `unsafeCoerce`. `inlineCleanup`
subsequently inlines these `unsafeCoerce` calls. The end result of all of this
is that we get no/fewer assignments in HDL where the RHS is simply a variable
reference. See issue #779.
-}

-- | Takes a binding and collapses its term if it is a noop. Only runs at
-- synthesis boundaries (NOINLINE/OPAQUE functions) to avoid running too early
-- on functions that might be inlined later. See #3036, and
-- Note [relation collapseRHSNoops and inlineCleanup].
collapseRHSNoops :: Transformation
collapseRHSNoops =
  toScopedTransformation "collapseRHSNoops" (onLet 'collapseRHSNoopsWorker)

-- | The 'Let' handler of 'collapseRHSNoops'
collapseRHSNoopsWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n -> Term n -> Bind n l -> Term l -> NormalizeSession (Term n)
collapseRHSNoopsWorker ctx letrec letBind body = do
  curBinding <- lookupBinding =<< curFunGlobal
  case curBinding of
    Just binding | isNoInline (bindingSpec binding) ->
      -- Keep the structure of the let-expression: a non-recursive binding
      -- stays non-recursive. See
      -- https://github.com/clash-lang/clash-compiler/issues/3044.
      case letBind of
        NonRec b0 e0 -> do
          (e1, Monoid.getAny -> collapsed) <-
            Writer.listen (runCollapseNoop sc (idBinderType b0) e0)
          pure (if collapsed then Let (NonRec b0 e1) body else letrec)
        Rec {} -> openLetBindings sc letBind body $ \scL bs lbs body1 -> do
          (es, Monoid.getAny -> collapsed) <-
            Writer.listen (mapM (\lb -> runCollapseNoop scL (lbType lb) (lbRhs lb)) lbs)
          pure (if collapsed then Let (Rec bs es) body1 else letrec)
    _ -> pure letrec
 where
  sc = tfScope ctx

  runCollapseNoop :: Distinct x => Scope x -> Type x -> Term x -> NormalizeSession (Term x)
  runCollapseNoop scX ty orig =
    runMaybeT (collapseNoop scX ty orig) >>= Maybe.maybe (return orig) changed

  collapseNoop :: Distinct x => Scope x -> Type x -> Term x -> MaybeT NormalizeSession (Term x)
  collapseNoop scX bTy term = do
    (Prim info, args) <- return (collectArgs term)
    identity <- getIdentity info (lefts args)
    te <- Lens.view typeEnv
    let aTy = inferCoreTypeOf te scX identity
    return (primUCo `TyApp` aTy `TyApp` bTy `App` identity)

  getIdentity :: PrimInfo -> [Term x] -> MaybeT NormalizeSession (Term x)
  getIdentity primInfo termArgs = do
    WorkIdentity idIdx noopIdxs <- return (primWorkInfo primInfo)
    mapM_ (getTermArg termArgs >=> isNoop >=> Monad.guard) noopIdxs
    getTermArg termArgs idIdx

  getTermArg :: [a] -> Int -> MaybeT NormalizeSession a
  getTermArg args i = do
    Monad.guard (i <= length args - 1)
    return (args !! i)

  -- Only global binders can be no-ops: a local variable can have the same
  -- unique as a global binder (#405), but it is a 'Var', not a 'GlobalVar'.
  isNoop :: Term x -> MaybeT NormalizeSession Bool
  isNoop = \case
    GlobalVar g -> do
      binding <- MaybeT (lookupBinding g)
      isRecursive <- lift (isRecursiveBndr g)
      Monad.guard (not isRecursive)
      isNoop (bindingTerm binding)
    Prim PrimInfo {primWorkInfo = WorkIdentity _ []} -> return True
    Lam x e -> isNoopApp (tmNameOf x) (collectArgs e)
    _ -> return False

  -- Check whether we have a term of the form:
  --
  -- primX a (primY b (primZ c (... x ...))))
  --
  -- Where primX, primY and primZ are either:
  --
  --  1. xToBV, or
  --  2. Primitives that are the identity on their argument
  --
  -- And that the variable 'x' is used by the last primitive in the chain.
  isNoopApp :: TmName x -> (Term x, [Either (Term x) (Type x)]) -> MaybeT NormalizeSession Bool
  isNoopApp x = \case
    (Var _ _ y, []) -> return (tmNameRaw x == tmNameRaw y)
    (Prim PrimInfo {primWorkInfo = WorkIdentity i []}, args) -> do
      arg <- getTermArg (lefts args) i
      isNoopApp x (collectArgs arg)
    (Prim PrimInfo {primName}, args)
      | primName == Text.showt 'BV.xToBV -> do
      -- We don't make 'xToBV' something of 'WorkIdentity 1 []' because we don't
      -- want 'getIdentity' to replace "naked" occurances of 'xToBV' by
      -- 'unsafeCoerce#'. We don't want that since 'xToBV' has a special evaluator
      -- rule that can translate XExceptions to 'undefined# :: BitVector n'.
      arg@(App {}) <- getTermArg (lefts args) 1
      isNoopApp x (collectArgs arg)
    _ -> return False
{-# SCC collapseRHSNoopsWorker #-}

-- * inlineNonRep

-- | Inline function with a non-representable result if it's the subject
-- of a Case-decomposition. It's a custom topdown traversal that -for efficiency
-- reasons- does not explore alternative of cases whose subject triggered an
-- 'inlineNonRepWorker'.
inlineNonRep
  :: (HasCallStack, Distinct n)
  => TransformContext n -> Term n -> NormalizeSession (Term n)
inlineNonRep ctx e0 = case e0 of
  Case {} -> do
    r <- Writer.listen (inlineNonRepWorker ctx e0)
    case r of
      (e1, Monoid.getAny -> True) ->
        return e1
      (e1, _) -> case e1 of
        -- If a term _in_ the subject triggers 'inlineNonRepWorker', inline and
        -- propagate might eliminate this case. We therefore don't explore the
        -- alternatives. Note that this makes it substantially different from
        -- a 'topdownSucR' transformation.
        Case subj0 ty alts ->
          Writer.listen (inlineNonRep (pushFrame CaseScrut ctx) subj0) >>= \case
            (subj1, Monoid.getAny -> True) ->
              return (Case subj1 ty alts)
            (subj1, _) -> do
              alts1 <- mapM (\alt -> underAlt ctx alt $ \ctxA p rhs ->
                                       Alt p <$> inlineNonRep ctxA rhs) alts
              pure (Case subj1 ty alts1)
        _ -> error ("internal error, inlineNonRep triggered on a non-Case:" <> showPpr e1)
  -- All non-case statements are simply traversed. TODO: are there other special
  -- cases like 'Case' that would warrant an optimization like ^ ?
  _ -> runScopedTransform (allR (ScopedTransform inlineNonRep)) ctx e0
{-# SCC inlineNonRep #-}

-- | Inline function with a non-representable result if it's the subject
-- of a Case-decomposition. This worker function only tries the given term
-- (i.e., it does not traverse it).
--
-- It sets the changed flag in the NormalizeSession if it successfully inlines
-- a binder.
inlineNonRepWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n -> Term n -> NormalizeSession (Term n)
inlineNonRepWorker ctx e = case e of
  Case scrut altsTy alts
    | (GlobalVar f, args, ticks) <- collectArgsTicks scrut -> do
      cf <- curFunGlobal
      isInlined <- zoomExtra (alreadyInlined f cf)
      limit <- Lens.view inlineLimit
      te <- Lens.view typeEnv
      let
        scrutTy = inferCoreTypeOf te (tfScope ctx) scrut

        -- Constraint dictionary inlining always terminates, so we ignore the
        -- usual inline safeguards.
        notClassTy = not (isClassTy te scrutTy)
        overLimit = notClassTy && Maybe.fromMaybe 0 isInlined > limit

      bodyMaybe <- lookupBinding f
      nonRepScrut <- isUntranslatableType False (tfScope ctx) scrutTy
      case (nonRepScrut, bodyMaybe) of
        (True, Just b) ->
          if overLimit then
            trace ($(curLoc) ++ [I.i|
              InlineNonRep: #{showPpr (globalIdName f)} already inlined
              #{limit} times in: #{showPpr (globalIdName cf)}. The type of the subject
              is:

                #{showPpr' def{displayTypes=True\} scrutTy}

              Function #{showPpr (globalIdName cf)} will not reach a normal form and
              compilation might fail.

              Run with '-fclash-inline-limit=N' to increase the inline limit to N.
            |]) (return e)
          else do
            Monad.when notClassTy (zoomExtra (addNewInline f cf))

            let scrutBody0 = mkTicks (sinkClosed (bindingTerm b)) (mkInlineTick f : ticks)
                scrutBody1 = mkApps scrutBody0 args

            changed (Case scrutBody1 altsTy alts)
        _ ->
          return e
  _ -> return e
{-# SCC inlineNonRepWorker #-}

-- * inlineSmall and inlineWorkFree

-- | True when @e@, appearing in context @ctx@, is an inner (partial) position
-- of a variable-headed application spine. That is, for a fully applied
-- function call:
--
--     f a b c
--
-- this function returns 'True' for:
--
--     f
--     f a
--     f a b
--
-- This is useful for functions such as 'inlineSmall' and 'inlineWorkFree'. The
-- former doesn't really care about arguments at all, but it is still useful to
-- only run once while traversing the tree. The latter will refuse to do any
-- work for partially applied functions in the first place -- but it only finds
-- out after performing relatively expensive checks.
--
-- Note that this will also return 'True' in the context of a tick ('KTickC').
-- That means that transformations using this as a performance guard should be
-- careful to use 'collectTicks' before matching on a constructor.
isPartOfVarAppSpine :: TransformContext n -> Term n -> Bool
isPartOfVarAppSpine ctx e = case ctxFrames ctx of
  k : _ -> isSpineKind k && isSpineNode e
  [] -> False
 where
  isSpineKind = \case
    KAppFun -> True
    KTyAppC -> True
    KTickC -> True
    _ -> False

  isSpineNode = \case
    Var {} -> True
    GlobalVar {} -> True
    App {} -> True
    TyApp {} -> True
    Tick {} -> True
    _ -> False

-- | Inline small functions
inlineSmall :: Transformation
inlineSmall =
  toScopedTransformation "inlineSmall"
    (onVarNode 'inlineSmallWorker <> onAppNode 'inlineSmallWorker
      <> onTyAppNode 'inlineSmallWorker <> onTickNode 'inlineSmallWorker)

-- | The application-spine handler of 'inlineSmall'
inlineSmallWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n -> Term n -> NormalizeSession (Term n)
inlineSmallWorker ctx e
  | isPartOfVarAppSpine ctx e
  = return e
  | (GlobalVar f, args, ticks) <- collectArgsTicks e
  = do
      -- XXX: This is deeply nested to short-circuit expensive checks
      topEnt <- isTopEntity f
      if topEnt
        then return e
        else do
          sizeLimit <- Lens.view inlineFunctionLimit
          lookupBinding f >>= \case
            Just b
              | not (isNoInline (bindingSpec b))
              , termSizeSmallerThan sizeLimit (bindingTerm b)
              -> do
                -- Don't inline recursive expressions
                isRecBndr <- isRecursiveBndr f
                if isRecBndr
                   then return e
                   else do
                     untranslatable <- isUntranslatable True (tfScope ctx) e
                     if untranslatable
                        then return e
                        else do
                          let tm = mkTicks (sinkClosed (bindingTerm b)) (mkInlineTick f : ticks)
                          changed (mkApps tm args)
            _ -> return e
  | otherwise
  = return e
{-# SCC inlineSmallWorker #-}

-- | Inline work-free functions, i.e. fully applied functions that evaluate to
-- a constant. Applications to closed arguments are normalized before they're
-- inlined, see 'normalizeWorkFreeApp'.
inlineWorkFree :: Transformation
inlineWorkFree =
  toScopedTransformation "inlineWorkFree"
    (onVarNode 'inlineWorkFreeWorker <> onAppNode 'inlineWorkFreeWorker
      <> onTyAppNode 'inlineWorkFreeWorker <> onTickNode 'inlineWorkFreeWorker)

-- | The application-spine handler of 'inlineWorkFree'
inlineWorkFreeWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n -> Term n -> NormalizeSession (Term n)
inlineWorkFreeWorker ctx e
  | isPartOfVarAppSpine ctx e
  = return e

  | (GlobalVar f, args@(_:_), ticks) <- collectArgsTicks e
  = do
    -- XXX: This is deeply nested to short-circuit expensive checks
    topEnt <- isTopEntity f
    if topEnt
      then return e
      else do
        lookupBinding f >>= \case
          Just b -> do
            te <- Lens.view typeEnv
            let eTy = inferCoreTypeOf te sc e
            if isSignalType te sc eTy
              then return e
              else do
                untranslatable <- isUntranslatableType True sc eTy
                argsHaveWork <- anyM (either expressionHasWork (const (pure False))) args
                if untranslatable || argsHaveWork
                  then return e
                  else do
                    -- Don't inline recursive expressions
                    isRecBndr <- isRecursiveBndr f
                    if | isRecBndr -> return e
                       -- Type arguments can still mention type variables bound
                       -- in the context, which rules out caching. Note that
                       -- closedness for the term is already checked in
                       -- 'expressionHasWork'.
                       | not (any hasFreeVarsType (rights args)) -> do
                         tm <- normalizeWorkFreeApp f b args
                         changed (mkTicks tm (mkInlineTick f : ticks))
                       | otherwise -> do
                         let tm = mkTicks (sinkClosed (bindingTerm b)) (mkInlineTick f : ticks)
                         changed (mkApps tm args)

          _ -> return e

  | (GlobalVar f, ticks) <- collectTicks e
  = do
      topEnt <- isTopEntity f
      te <- Lens.view typeEnv
      let fTy = sinkClosed (globalIdType f)
          closed = not (isPolyFunCoreTy te sc fTy)
      if topEnt || not closed || isSignalType te sc fTy
        then return e
        else do
          untranslatable <- isUntranslatableType True sc fTy
          if untranslatable
            then return e
            else do
              lookupBinding f >>= \case
                -- Don't inline recursive expressions
                Just top -> do
                  isRecBndr <- isRecursiveBndr f
                  if isRecBndr
                     then return e
                     else do
                      let topB = bindingTerm top
                      sizeLimit <- Lens.view inlineWFCacheLimit
                      -- caching only worth it from a certain size onwards, otherwise
                      -- the caching mechanism itself brings more of an overhead.
                      if termSizeSmallerThan sizeLimit topB then
                        changed (mkTicks (sinkClosed topB) ticks)
                      else do
                        b <- normalizeTopLvlBndr False f top
                        changed (mkTicks (sinkClosed (bindingTerm b)) ticks)
                _ -> return e

  | otherwise
  = return e
 where
  sc = tfScope ctx

  -- an expression has work when it contains free local variables,
  -- or has a Signal type, i.e. it does not evaluate to a work-free
  -- constant. The free-variable check runs first: it is cheaper than
  -- inferring the expression's type.
  expressionHasWork e' =
    if not (isClosed e')
      then return True
      else do
        te <- Lens.view typeEnv
        return (isSignalType te sc (inferCoreTypeOf te sc e'))
{-# SCC inlineWorkFreeWorker #-}
