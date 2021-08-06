{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021     , QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformations for inlining
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Normalize.Transformations.Inline
  ( bindConstantVar
  , inlineBndrsCleanup
  , inlineCast
  , inlineCleanup
  , inlineHO
  , collapseRHSNoops
  , inlineNonRep
  , inlineOrLiftNonRep
  , inlineSimIO
  , inlineSmall
  , inlineWorkFree
  ) where

import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Writer ((>=>),lift,listen)
import Data.Default (Default(..))
import Data.Either  (lefts)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid (Any(..))
import qualified Data.Text as Text
import qualified Data.Text.Extra as Text
import GHC.Stack (HasCallStack)

#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.Basic             (InlineSpec (..))
#else
import BasicTypes                  (InlineSpec (..))
#endif

import qualified Clash.Explicit.SimIO as SimIO
import qualified Clash.Sized.Internal.BitVector as BV (Bit(Bit), BitVector(BV))

import Clash.Annotations.Primitive (extractPrim)
import Clash.Core.DataCon (DataCon(..))
import Clash.Core.FreeVars
  (countFreeOccurances, freeLocalIds, localIdDoesNotOccurIn)
import Clash.Core.Name (Name(..), NameSort(..))
import Clash.Core.Pretty (PrettyOptions(..), showPpr, showPpr')
import Clash.Core.Subst
import Clash.Core.Term
  ( CoreContext(..), Pat(..), PrimInfo(..), Term(..), WorkInfo(..), collectArgs
  , collectArgsTicks, mkApps , mkTicks, stripTicks)
import Clash.Core.TermInfo (isLocalVar, isPolyFun, termSize, termType)
import Clash.Core.Type
  (TypeView(..), isClassTy, isPolyFunCoreTy, tyView)
import Clash.Core.Util (isSignalType, primUCo)
import Clash.Core.Var (Id, Var(..), isGlobalId, isLocalId)
import Clash.Core.VarEnv
  ( InScopeSet, VarEnv, VarSet, elemUniqInScopeSet, elemVarEnv, elemVarSet
  , eltsVarEnv, emptyVarEnv, extendInScopeSetList, extendVarEnv
  , foldlWithUniqueVarEnv', lookupVarEnv, lookupVarEnvDirectly, mkVarEnv
  , notElemVarSet, unionVarEnv, unionVarEnvWith, unitVarSet)
import Clash.Debug (trace, traceIf)
import Clash.Driver.Types (Binding(..), DebugOpts(dbg_invariants))
import Clash.Netlist.Util (representableType)
import Clash.Primitives.Types
  (CompiledPrimMap, Primitive(..), TemplateKind(..))
import Clash.Rewrite.Combinators (allR)
import Clash.Rewrite.Types
  ( TransformContext(..), bindings, curFun, customReprs, debugOpts, extra
  , tcCache, topEntities, typeTranslator)
import Clash.Rewrite.Util
  ( changed, inlineBinders, inlineOrLiftBinders, isJoinPointIn
  , isUntranslatable, isUntranslatableType, isVoidWrapper, zoomExtra)
import Clash.Rewrite.WorkFree (isWorkFreeIsh)
import Clash.Normalize.Types
  ( NormRewrite, NormalizeSession, inlineConstantLimit, inlineFunctionLimit
  , inlineLimit, inlineWFCacheLimit, primitives)
import Clash.Normalize.Util
  ( addNewInline, alreadyInlined, isRecursiveBndr, mkInlineTick
  , normalizeTopLvlBndr)
import Clash.Unique (Unique)
import Clash.Util (curLoc)
import qualified Clash.Util.Interpolate as I

{- [Note] join points and void wrappers
Join points are functions that only occur in tail-call positions within an
expression, and only when they occur in a tail-call position more than once.
Normally bindNonRep binds/inlines all non-recursive local functions. However,
doing so for join points would significantly increase compilation time, so we
avoid it. The only exception to this rule are so-called void wrappers. Void
wrappers are functions of the form:
> \(w :: Void) -> f a b c
i.e. a wrapper around the function 'f' where the argument 'w' is not used. We
do bind/line these join-points because these void-wrappers interfere with the
'disjoint expression consolidation' (DEC) and 'common sub-expression elimination'
(CSE) transformation, sometimes resulting in circuits that are twice as big
as they'd need to be.
-}

-- | Inline let-bindings when the RHS is either a local variable reference or
-- is constant (except clock or reset generators)
bindConstantVar :: HasCallStack => NormRewrite
bindConstantVar = inlineBinders test
  where
    test _ (i,stripTicks -> e) = case isLocalVar e of
      -- Don't inline `let x = x in x`, it throws  us in an infinite loop
      True -> return (i `localIdDoesNotOccurIn` e)
      _    -> do
        tcm <- Lens.view tcCache
        case isWorkFreeIsh tcm e of
          True -> Lens.use (extra.inlineConstantLimit) >>= \case
            0 -> return True
            n -> return (termSize e <= n)
          _ -> return False
{-# SCC bindConstantVar #-}

-- | Mark to track progress of 'reduceBindersCleanup'
data Mark = Temp | Done | Rec

-- | Used (transitively) by 'inlineCleanup' inline to-inline let-binders into
-- the other to-inline let-binders.
reduceBindersCleanup
  :: HasCallStack
  => InScopeSet
  -- ^ Current InScopeSet
  -> VarEnv ((Id,Term),VarEnv Int)
  -- ^ Original let-binders with their free variables (+ #occurrences)
  -> (Maybe Subst,VarEnv Int,VarEnv ((Id,Term),VarEnv Int,Mark))
  -- ^ Accumulated:
  --
  -- 1. (Maybe) the build up substitution so far
  -- 2. The free variables of the range of the substitution
  -- 3. Processed let-binders with their free variables and a tag to mark
  --    the progress:
  --    * Temp: Will eventually form a recursive cycle
  --    * Done: Processed, non-recursive
  --    * Rec:  Processed, recursive
  -> Unique
  -- ^ The unique of the let-binding that we want to simplify
  -> Int
  -- ^ Ignore, artifact of 'foldlWithUniqueVarEnv'
  -> (Maybe Subst,VarEnv Int,VarEnv ((Id,Term),VarEnv Int,Mark))
  -- ^ Same as the third argument
reduceBindersCleanup isN origInl (!substM,!substFVs,!doneInl) u _ =
  case lookupVarEnvDirectly u doneInl of
    Nothing -> case lookupVarEnvDirectly u origInl of
      Nothing ->
        -- let-binding not found, cannot extend the substitution
        if elemUniqInScopeSet u isN then
          (substM,substFVs,doneInl)
        else
          error [I.i|
            Internal error: 'reduceBindersCleanup' encountered a variable
            reference that was neither in 'doneInl', 'origInl', or in the
            transformation's in scope set. Unique was: '#{u}'.
          |]
      Just ((v,e),eFVs) ->
        -- Simplify the transitive dependencies
        let (sM,substFVsE,doneInl1) =
              foldlWithUniqueVarEnv'
                (reduceBindersCleanup isN origInl)
                ( Nothing
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
                , extendVarEnv v ((v,e),eFVs,Temp) doneInl)
                eFVs

            e1 = maybeSubstTm "reduceBindersCleanup" sM e
        in  if v `elemVarEnv` substFVsE then
              -- We cannot inline recursive let-bindings, so we do not extend
              -- the substitution environment.
              ( substM
              , substFVs
              -- And we explicitly mark the let-binding as recursive in the
              -- processing environment. So that it will be kept around at the
              -- end of 'inlineCleanup'
              , extendVarEnv v ((v,e1),substFVsE,Rec) doneInl1
              )
            else
              -- Extend the substitution
              ( Just (extendIdSubst (Maybe.fromMaybe (mkSubst isN) substM) v e1)
              , unionVarEnv substFVsE substFVs
              -- Mark the let-binding a fully "reduced", so we don't repeat
              -- this process when we encounter it again.
              , extendVarEnv v ((v,e1),substFVsE,Done) doneInl1
              )
    -- It's already been processed, just extend the substitution environment
    Just ((v,e),eFVs,Done) ->
      ( Just (extendIdSubst (Maybe.fromMaybe (mkSubst isN) substM) v e)
      , unionVarEnv eFVs substFVs
      , doneInl
      )

    -- It's either recursive (Rec), or part of a recursive group (Temp) where we
    -- originally entered a different part of the cycle. Regardless, we do not
    -- extend the substitution environment.
    Just _ ->
      ( substM
      , substFVs
      , doneInl
      )
{-# SCC reduceBindersCleanup #-}

-- | Used by 'inlineCleanup' to inline binders that we want to inline into the
-- binders that we want to keep.
inlineBndrsCleanup
  :: HasCallStack
  => InScopeSet
  -- ^ Current InScopeSet
  -> VarEnv ((Id,Term),VarEnv Int)
  -- ^ Original let-binders with their free variables (+ #occurrences), that we
  -- want to inline
  -> VarEnv ((Id,Term),VarEnv Int,Mark)
  -- ^ Processed let-binders with their free variables and a tag to mark the
  -- progress:
  --   * Temp: Will eventually form a recursive cycle
  --   * Done: Processed, non-recursive
  --   * Rec:  Processed, recursive
  -> [((Id,Term),VarEnv Int)]
  -- ^ The let-binders with their free variables (+ #occurrences), that we want
  -- to keep
  -> [(Id,Term)]
inlineBndrsCleanup isN origInl = go
 where
  go doneInl [] =
    -- If some of the let-binders that we wanted to inline turn out to be
    -- recursive, then we have to keep those around as well, as we weren't able
    -- to inline them. Furthermore, for every recursive binder there might still
    -- be non-inlined variables left, see #1337.
    flip map [ (ve, eFvs) | (ve,eFvs,Rec) <- eltsVarEnv doneInl ] $ \((v, e), eFvs) ->
      let
        (substM, _, _) = foldlWithUniqueVarEnv'
                           (reduceBindersCleanup isN emptyVarEnv)
                           (Nothing, emptyVarEnv, doneInl)
                           eFvs
      in (v, maybeSubstTm "inlineBndrsCleanup_0" substM e)
  go !doneInl_0 (((v,e),eFVs):il) =
    let (sM,_,doneInl_1) = foldlWithUniqueVarEnv'
                            (reduceBindersCleanup isN origInl)
                            (Nothing, emptyVarEnv, doneInl_0)
                            eFVs
        e1 = maybeSubstTm "inlineBndrsCleanup_1" sM e
    in  (v,e1):go doneInl_1 il
{-# SCC inlineBndrsCleanup #-}

-- | Only inline casts that just contain a 'Var', because these are guaranteed work-free.
-- These are the result of the 'splitCastWork' transformation.
inlineCast :: HasCallStack => NormRewrite
inlineCast = inlineBinders test
  where
    test _ (_, (Cast (stripTicks -> Var {}) _ _)) = return True
    test _ _ = return False
{-# SCC inlineCast #-}

-- | Given a function in the desired normal form, inline all the following
-- let-bindings:
--
-- Let-bindings with an internal name that is only used once, where it binds:
--   * a primitive that will be translated to an HDL expression (as opposed to
--     a HDL declaration)
--   * a projection case-expression (1 alternative)
--   * a data constructor
--   * I/O actions
inlineCleanup :: HasCallStack => NormRewrite
inlineCleanup (TransformContext is0 _) (Letrec binds body) = do
  prims <- Lens.use (extra.primitives)
  -- For all let-bindings, count the number of times they are referenced.
  -- We only inline let-bindings which are referenced only once, otherwise
  -- we would lose sharing.
  let is1       = extendInScopeSetList is0 (map fst binds)
      bindsFvs  = map (\(v,e) -> (v,((v,e),countFreeOccurances e))) binds
      allOccs   = List.foldl' (unionVarEnvWith (+)) emptyVarEnv
                $ map (snd.snd) bindsFvs
      bodyFVs   = Lens.foldMapOf freeLocalIds unitVarSet body
      (il,keep) = List.partition (isInteresting allOccs prims bodyFVs)
                                 bindsFvs
      keep'     = inlineBndrsCleanup is1 (mkVarEnv il) emptyVarEnv
                $ map snd keep

  if | null il -> return  (Letrec binds body)
     | null keep' -> changed body
     | otherwise -> changed (Letrec keep' body)
  where
    -- Determine whether a let-binding is interesting to inline
    isInteresting
      :: VarEnv Int
      -> CompiledPrimMap
      -> VarSet
      -> (Id,((Id, Term), VarEnv Int))
      -> Bool
    isInteresting allOccs prims bodyFVs (id_,((_,(fst.collectArgs) -> tm),_))
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
      | nameSort (varName id_) /= User
      , id_ `notElemVarSet` bodyFVs
      = case tm of
          Prim pInfo
            | let nm = primName pInfo
            , Just (extractPrim -> Just p@(BlackBox {})) <- HashMap.lookup nm prims
            , TExpr <- kind p
            , Just occ <- lookupVarEnv id_ allOccs
            , occ < 2
            -> True
            | otherwise
            -> primName pInfo `elem` ["Clash.Explicit.SimIO.bindSimIO#"]
          Case _ _ [_] -> True
          Data _ -> True
          Case _ aTy (_:_:_)
            | TyConApp nm _ <- tyView aTy
            , nameOcc nm == Text.showt ''SimIO.SimIO
            -> True
          _ -> False
      | id_ `notElemVarSet` bodyFVs
      = case tm of
          Prim pInfo
            | primName pInfo `elem`
                        [ Text.showt 'SimIO.openFile
                        , Text.showt 'SimIO.getChar
                        , Text.showt 'SimIO.isEOF
                        ]
            , Just occ <- lookupVarEnv id_ allOccs
            , occ < 2
            -> True
            | otherwise
            -> primName pInfo `elem` ["Clash.Explicit.SimIO.bindSimIO#"]
          Case _ _ [(DataPat dcE _ _,_)]
            -> let nm = (nameOcc (dcName dcE))
               in -- Inlines WW projection that exposes internals of the BitVector types
                  nm == Text.showt 'BV.BV  ||
                  nm == Text.showt 'BV.Bit ||
                  -- Inlines projections out of constraint-tuples (e.g. HiddenClockReset)
                  "GHC.Classes" `Text.isPrefixOf` nm
          Case _ aTy (_:_:_)
            | TyConApp nm _ <- tyView aTy
            , nameOcc nm == Text.showt ''SimIO.SimIO
            -> True
          _ -> False

    isInteresting _ _ _ _ = False

inlineCleanup _ e = return e
{-# SCC inlineCleanup #-}

{- [Note] relation `collapseRHSNoops` and `inlineCleanup`
The `collapseRHSNoops` transformation replaces functions/primitives that are the identity
in HDL, but not in Haskell, by `unsafeCoerce`.
`inlineCleanup` subsequently inlines these `unsafeCoerce` calls.
The end result of all of this is that we get no/fewer assignments in HDL where the RHS is
simply a variable reference. See issue #779 -}

-- | Takes a binding and collapses its term if it is a noop
collapseRHSNoops :: HasCallStack => NormRewrite
collapseRHSNoops _ (Letrec binds body) = do
  binds1 <- mapM runCollapseNoop binds
  return $ Letrec binds1 body
  where
    runCollapseNoop orig =
      runMaybeT (collapseNoop orig) >>= Maybe.maybe (return orig) changed

    collapseNoop (iD,term) = do
      (Prim info,args) <- return $ collectArgs term
      identity         <- getIdentity info $ lefts args
      collapsed        <- collapseToIdentity iD identity
      return (iD,collapsed)

    collapseToIdentity iD identity = do
      tcm <- Lens.view tcCache
      let aTy = termType tcm identity
          bTy = varType iD
      return $ primUCo `TyApp` aTy `TyApp` bTy `App` identity

    getIdentity primInfo termArgs = do
      WorkIdentity idIdx noopIdxs <- return $ primWorkInfo primInfo
      mapM_ (getTermArg termArgs >=> isNoop >=> Monad.guard) noopIdxs
      getTermArg termArgs idIdx

    getTermArg args i = do
      Monad.guard $ i <= length args - 1
      return $ args !! i

    isNoop (Var i) = do
      binding     <- MaybeT $ lookupVarEnv i <$> Lens.use bindings
      isRecursive <- lift $ isRecursiveBndr $ bindingId binding
      Monad.guard $ not isRecursive
      isNoop $ bindingTerm binding
    isNoop (Prim PrimInfo{primWorkInfo=WorkIdentity _ []}) = return True
    isNoop _ = return False

collapseRHSNoops _ e = return e
{-# SCC collapseRHSNoops #-}

-- | Inline a function with functional arguments
inlineHO :: HasCallStack => NormRewrite
inlineHO _ e@(App _ _)
  | (Var f, args, ticks) <- collectArgsTicks e
  = do
    tcm <- Lens.view tcCache
    let hasPolyFunArgs = or (map (either (isPolyFun tcm) (const False)) args)
    if hasPolyFunArgs
      then do (cf,_)    <- Lens.use curFun
              isInlined <- zoomExtra (alreadyInlined f cf)
              limit     <- Lens.use (extra.inlineLimit)
              if (Maybe.fromMaybe 0 isInlined) > limit
                then do
                  opts <- Lens.view debugOpts
                  traceIf (dbg_invariants opts) ($(curLoc) ++ "InlineHO: " ++ show f ++ " already inlined " ++ show limit ++ " times in:" ++ show cf) (return e)
                else do
                  bodyMaybe <- lookupVarEnv f <$> Lens.use bindings
                  case bodyMaybe of
                    Just b -> do
                      zoomExtra (addNewInline f cf)
                      changed (mkApps (mkTicks (bindingTerm b) ticks) args)
                    _ -> return e
      else return e

inlineHO _ e = return e
{-# SCC inlineHO #-}

-- | Inline function with a non-representable result if it's the subject
-- of a Case-decomposition. It's a custom topdown traversal that -for efficiency
-- reasons- does not explore alternative of cases whose subject triggered an
-- 'inlineNonRepWorker'.
inlineNonRep :: HasCallStack => NormRewrite
inlineNonRep ctx0 e0@(Case {}) = do
  r <- listen (inlineNonRepWorker e0)
  case r of
    (e1, Monoid.getAny -> True) ->
      return e1
    (~(Case subj0 typ alts), _) -> do
      -- If a term _in_ the subject triggers 'inlineNonRepWorker', inline and
      -- propagate might eliminate this case. We therefore don't explore the
      -- alternatives. Note that this makes it substantially different from a
      -- 'topdownSucR' transformation.
      let
        TransformContext inScope ctx1 = ctx0
        ctx2 = TransformContext inScope (CaseScrut:ctx1)

      listen (inlineNonRep ctx2 subj0) >>= \case
        (subj1, Monoid.getAny -> True) ->
          return (Case subj1 typ alts)
        (subj1, _) -> do
          let (pats, rhss0) = unzip alts
          rhss1 <- mapM (inlineNonRep ctx2) rhss0
          pure (Case subj1 typ (zip pats rhss1))

inlineNonRep ctx e =
  -- All non-case statements are simply traversed. TODO: are there other special
  -- cases like 'Case' that would warrant an optimization like ^ ?
  allR inlineNonRep ctx e
{-# SCC inlineNonRep #-}

-- | Inline function with a non-representable result if it's the subject
-- of a Case-decomposition. This worker function only tries the given term
-- (i.e., it does not traverse it).
--
-- It sets the changed flag in the NormalizeSession if it successfully inlines
-- a binder.
inlineNonRepWorker :: HasCallStack => Term -> NormalizeSession Term
inlineNonRepWorker e@(Case scrut altsTy alts)
  | (Var f, args,ticks) <- collectArgsTicks scrut
  , isGlobalId f
  = do
    (cf,_)    <- Lens.use curFun
    isInlined <- zoomExtra (alreadyInlined f cf)
    limit     <- Lens.use (extra.inlineLimit)
    tcm       <- Lens.view tcCache
    let
      scrutTy = termType tcm scrut

      -- Constraint dictionary inlining always terminates, so we ignore the
      -- usual inline safeguards.
      notClassTy = not (isClassTy tcm scrutTy)
      overLimit = notClassTy && (Maybe.fromMaybe 0 isInlined) > limit


    bodyMaybe   <- lookupVarEnv f <$> Lens.use bindings
    nonRepScrut <- not <$> (representableType <$> Lens.view typeTranslator
                                              <*> Lens.view customReprs
                                              <*> pure False
                                              <*> Lens.view tcCache
                                              <*> pure scrutTy)
    case (nonRepScrut, bodyMaybe) of
      (True, Just b) -> do
        if overLimit then
          trace ($(curLoc) ++ [I.i|
            InlineNonRep: #{showPpr (varName f)} already inlined
            #{limit} times in: #{showPpr (varName cf)}. The type of the subject
            is:

              #{showPpr' def{displayTypes=True\} scrutTy}

            Function #{showPpr (varName cf)} will not reach a normal form and
            compilation might fail.

            Run with '-fclash-inline-limit=N' to increase the inline limit to N.
          |]) (return e)
        else do
          Monad.when notClassTy (zoomExtra (addNewInline f cf))

          let scrutBody0 = mkTicks (bindingTerm b) (mkInlineTick f : ticks)
          let scrutBody1 = mkApps scrutBody0 args

          changed $ Case scrutBody1 altsTy alts
      _ ->
        return e

inlineNonRepWorker e = pure e
{-# SCC inlineNonRepWorker #-}

inlineOrLiftNonRep :: HasCallStack => NormRewrite
inlineOrLiftNonRep ctx eLet@(Letrec _ body) =
    inlineOrLiftBinders nonRepTest inlineTest ctx eLet
  where
    bodyFreeOccs = countFreeOccurances body

    nonRepTest :: (Id, Term) -> NormalizeSession Bool
    nonRepTest (Id {varType = ty}, _)
      = not <$> (representableType <$> Lens.view typeTranslator
                                   <*> Lens.view customReprs
                                   <*> pure False
                                   <*> Lens.view tcCache
                                   <*> pure ty)
    nonRepTest _ = return False

    inlineTest :: Term -> (Id, Term) -> Bool
    inlineTest e (id_, e') =
      -- We do __NOT__ inline:
      not $ or
        [ -- 1. recursive let-binders
          -- id_ `localIdOccursIn` e' -- <= already checked in inlineOrLiftBinders
          -- 2. join points (which are not void-wrappers)
          isJoinPointIn id_ e && not (isVoidWrapper e')
          -- 3. binders that are used more than once in the body, because
          --    it makes CSE a whole lot more difficult.
          --
          -- XXX: Check whether we can extend this to the binders as well
        , maybe False (>1) (lookupVarEnv id_ bodyFreeOccs)
        ]

inlineOrLiftNonRep _ e = return e
{-# SCC inlineOrLiftNonRep #-}

-- | Inline anything of type `SimIO`: IO actions cannot be shared
inlineSimIO :: HasCallStack => NormRewrite
inlineSimIO = inlineBinders test
  where
    test _ (i,_) = case tyView (varType i) of
      TyConApp tc _ -> return $! nameOcc tc == Text.showt ''SimIO.SimIO
      _ -> return False
{-# SCC inlineSimIO #-}

-- | Inline small functions
inlineSmall :: HasCallStack => NormRewrite
inlineSmall _ e@(collectArgsTicks -> (Var f,args,ticks)) = do
  untranslatable <- isUntranslatable True e
  topEnts <- Lens.view topEntities
  let lv = isLocalId f
  if untranslatable || f `elemVarSet` topEnts || lv
    then return e
    else do
      bndrs <- Lens.use bindings
      sizeLimit <- Lens.use (extra.inlineFunctionLimit)
      case lookupVarEnv f bndrs of
        -- Don't inline recursive expressions
        Just b -> do
          isRecBndr <- isRecursiveBndr f
          if not isRecBndr && bindingSpec b /= NoInline && termSize (bindingTerm b) < sizeLimit
             then do
               let tm = mkTicks (bindingTerm b) (mkInlineTick f : ticks)
               changed $ mkApps tm args
             else return e

        _ -> return e

inlineSmall _ e = return e
{-# SCC inlineSmall #-}

-- | Inline work-free functions, i.e. fully applied functions that evaluate to
-- a constant
inlineWorkFree :: HasCallStack => NormRewrite
inlineWorkFree _ e@(collectArgsTicks -> (Var f,args@(_:_),ticks))
  = do
    tcm <- Lens.view tcCache
    let eTy = termType tcm e
    argsHaveWork <- or <$> mapM (either expressionHasWork
                                        (const (pure False)))
                                args
    untranslatable <- isUntranslatableType True eTy
    topEnts <- Lens.view topEntities
    let isSignal = isSignalType tcm eTy
    let lv = isLocalId f
    let isTopEnt = elemVarSet f topEnts
    if untranslatable || isSignal || argsHaveWork || lv || isTopEnt
      then return e
      else do
        bndrs <- Lens.use bindings
        case lookupVarEnv f bndrs of
          -- Don't inline recursive expressions
          Just b -> do
            isRecBndr <- isRecursiveBndr f
            if isRecBndr
               then return e
               else do
                 let tm = mkTicks (bindingTerm b) (mkInlineTick f : ticks)
                 changed $ mkApps tm args

          _ -> return e
  where
    -- an expression is has work when it contains free local variables,
    -- or has a Signal type, i.e. it does not evaluate to a work-free
    -- constant.
    expressionHasWork e' = do
      let fvIds = Lens.toListOf freeLocalIds e'
      tcm   <- Lens.view tcCache
      let e'Ty     = termType tcm e'
          isSignal = isSignalType tcm e'Ty
      return (not (null fvIds) || isSignal)

inlineWorkFree _ e@(Var f) = do
  tcm <- Lens.view tcCache
  let fTy      = varType f
      closed   = not (isPolyFunCoreTy tcm fTy)
      isSignal = isSignalType tcm fTy
  untranslatable <- isUntranslatableType True fTy
  topEnts <- Lens.view topEntities
  let gv = isGlobalId f
  if closed && f `notElemVarSet` topEnts && not untranslatable && not isSignal && gv
    then do
      bndrs <- Lens.use bindings
      case lookupVarEnv f bndrs of
        -- Don't inline recursive expressions
        Just top -> do
          isRecBndr <- isRecursiveBndr f
          if isRecBndr
             then return e
             else do
              let topB = bindingTerm top
              sizeLimit <- Lens.use (extra.inlineWFCacheLimit)
              -- caching only worth it from a certain size onwards, otherwise
              -- the caching mechanism itself brings more of an overhead.
              if termSize topB < sizeLimit then
                changed topB
              else do
                b <- normalizeTopLvlBndr False f top
                changed (bindingTerm b)
        _ -> return e
    else return e

inlineWorkFree _ e = return e
{-# SCC inlineWorkFree #-}
