{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Transformations of the Normalization process
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Normalize.Transformations
  ( module X
  , inlineNonRep
  , inlineOrLiftNonRep
  , typeSpec
  , nonRepSpec
  , etaExpansionTL
  , nonRepANF
  , bindConstantVar
  , constantSpec
  , makeANF
  , inlineWorkFree
  , inlineHO
  , inlineSmall
  , inlineCleanup
  , inlineBndrsCleanup
  , splitCastWork
  , inlineCast
  , caseCast
  , letCast
  , eliminateCastCast
  , argCastSpec
  , etaExpandSyn
  , appPropFast
  , separateArguments
  , separateLambda
  , xOptimize
  , setupMultiResultPrim
  , inlineSimIO
  )
where

import           Control.Arrow               ((***))
import           Control.Exception           (throw)
import           Control.Lens                (_2)
import qualified Control.Lens                as Lens
import qualified Control.Monad               as Monad
import           Control.Monad.Extra         (orM)
import           Control.Monad.State         (StateT (..), modify)
import           Control.Monad.Writer        (lift, listen)
import           Data.Bifunctor              (second)
import           Data.Default
import qualified Data.Either                 as Either
import qualified Data.HashMap.Lazy           as HashMap
import qualified Data.List                   as List
import qualified Data.List.Extra             as List
import qualified Data.Maybe                  as Maybe
import qualified Data.Monoid                 as Monoid
import qualified Data.Text                   as Text
import           Data.Text.Extra             (showt)
import           GHC.Stack                   (HasCallStack)

#if MIN_VERSION_ghc(9,0,0)
import           GHC.Types.Basic             (InlineSpec (..))
#else
import           BasicTypes                  (InlineSpec (..))
#endif

import           Clash.Annotations.Primitive (extractPrim)
import           Clash.Core.DataCon          (DataCon (..))
import           Clash.Core.Name
  (mkUnsafeInternalName, Name (..), NameSort (..), mkUnsafeSystemName, nameOcc)
import           Clash.Core.FreeVars
  (localIdsDoNotOccurIn, freeLocalIds, termFreeTyVars,
   typeFreeVars, localIdDoesNotOccurIn, countFreeOccurances)
import           Clash.Core.Pretty           (PrettyOptions(..), showPpr, showPpr')
import           Clash.Core.Subst
import           Clash.Core.Term
import           Clash.Core.TermInfo
import           Clash.Core.Type             (Type (..), TypeView (..), applyFunTy,
                                              isPolyFunCoreTy, isClassTy,
                                              normalizeType, splitFunForallTy,
                                              splitFunTy,
                                              tyView, mkPolyFunTy, coreView)
import           Clash.Core.TyCon            (TyConMap)
import           Clash.Core.Util
  (Projections (..), isSignalType, shouldSplit, mkInternalVar, mkSelectorCase)
import           Clash.Core.Var
  (Id, TyVar, Var (..), isGlobalId, isLocalId, mkLocalId)
import           Clash.Core.VarEnv
  (InScopeSet, VarEnv, VarSet, elemVarSet,
   emptyVarEnv, extendInScopeSet, extendInScopeSetList, lookupVarEnv,
   notElemVarSet, unionVarEnvWith, unionInScope, unitVarSet, mkVarSet,
   mkInScopeSet, uniqAway, elemVarEnv, foldlWithUniqueVarEnv',
   lookupVarEnvDirectly, extendVarEnv, unionVarEnv, eltsVarEnv, mkVarEnv,
   elemUniqInScopeSet)
import           Clash.Debug
import           Clash.Driver.Types          (Binding(..), DebugLevel (..))
import           Clash.Netlist.BlackBox.Types (Element(Err))
import           Clash.Netlist.Types         (BlackBox(..))
import           Clash.Netlist.Util (representableType, bindsExistentials)
import Clash.Normalize.Transformations.Case as X
import Clash.Normalize.Transformations.DEC as X
import Clash.Normalize.Transformations.Letrec as X
import Clash.Normalize.Transformations.Reduce as X
import           Clash.Normalize.Types
import           Clash.Normalize.Util
import           Clash.Primitives.Types
  (Primitive(..), TemplateKind(TExpr), CompiledPrimMap)
import           Clash.Rewrite.Combinators
import           Clash.Rewrite.Types
import           Clash.Rewrite.Util
import           Clash.Unique
import           Clash.Util
import qualified Clash.Util.Interpolate as I

inlineOrLiftNonRep :: HasCallStack => NormRewrite
inlineOrLiftNonRep ctx eLet@(Letrec _ body) =
    inlineOrLiftBinders nonRepTest inlineTest ctx eLet
  where
    bodyFreeOccs = countFreeOccurances body

    nonRepTest :: (Id, Term) -> RewriteMonad extra Bool
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

-- | Specialize functions on their type
typeSpec :: HasCallStack => NormRewrite
typeSpec ctx e@(TyApp e1 ty)
  | (Var {},  args) <- collectArgs e1
  , null $ Lens.toListOf typeFreeVars ty
  , (_, []) <- Either.partitionEithers args
  = specializeNorm ctx e

typeSpec _ e = return e
{-# SCC typeSpec #-}

-- | Specialize functions on their non-representable argument
nonRepSpec :: HasCallStack => NormRewrite
nonRepSpec ctx e@(App e1 e2)
  | (Var {}, args) <- collectArgs e1
  , (_, [])     <- Either.partitionEithers args
  , null $ Lens.toListOf termFreeTyVars e2
  = do tcm <- Lens.view tcCache
       let e2Ty = termType tcm e2
       let localVar = isLocalVar e2
       nonRepE2 <- not <$> (representableType <$> Lens.view typeTranslator
                                              <*> Lens.view customReprs
                                              <*> pure False
                                              <*> Lens.view tcCache
                                              <*> pure e2Ty)
       if nonRepE2 && not localVar
         then do
           e2' <- inlineInternalSpecialisationArgument e2
           specializeNorm ctx (App e1 e2')
         else return e
  where
    -- | If the argument on which we're specialising ia an internal function,
    -- one created by the compiler, then inline that function before we
    -- specialise.
    --
    -- We need to do this because otherwise the specialisation history won't
    -- recognize the new specialisation argument as something the function has
    -- already been specialized on
    inlineInternalSpecialisationArgument
      :: Term
      -> NormalizeSession Term
    inlineInternalSpecialisationArgument app
      | (Var f,fArgs,ticks) <- collectArgsTicks app
      = do
        fTmM <- lookupVarEnv f <$> Lens.use bindings
        case fTmM of
          Just b
            | nameSort (varName (bindingId b)) == Internal
            -> censor (const mempty)
                      (topdownR appPropFast ctx
                        (mkApps (mkTicks (bindingTerm b) ticks) fArgs))
          _ -> return app
      | otherwise = return app

nonRepSpec _ e = return e
{-# SCC nonRepSpec #-}

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


-- | Bring an application of a DataCon or Primitive in ANF, when the argument is
-- is considered non-representable
nonRepANF :: HasCallStack => NormRewrite
nonRepANF ctx@(TransformContext is0 _) e@(App appConPrim arg)
  | (conPrim, _) <- collectArgs e
  , isCon conPrim || isPrim conPrim
  = do
    untranslatable <- isUntranslatable False arg
    case (untranslatable,stripTicks arg) of
      (True,Letrec binds body) ->
        -- This is a situation similar to Note [CaseLet deshadow]
        let (binds1,body1) = deshadowLetExpr is0 binds body
        in  changed (Letrec binds1 (App appConPrim body1))
      (True,Case {})  -> specializeNorm ctx e
      (True,Lam {})   -> specializeNorm ctx e
      (True,TyLam {}) -> specializeNorm ctx e
      _               -> return e

nonRepANF _ e = return e
{-# SCC nonRepANF #-}

-- Misc rewrites

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

-- | Push a cast over a case into it's alternatives.
caseCast :: HasCallStack => NormRewrite
caseCast _ (Cast (stripTicks -> Case subj ty alts) ty1 ty2) = do
  let alts' = map (\(p,e) -> (p, Cast e ty1 ty2)) alts
  changed (Case subj ty alts')
caseCast _ e = return e
{-# SCC caseCast #-}


-- | Push a cast over a Letrec into it's body
letCast :: HasCallStack => NormRewrite
letCast _ (Cast (stripTicks -> Letrec binds body) ty1 ty2) =
  changed $ Letrec binds (Cast body ty1 ty2)
letCast _ e = return e
{-# SCC letCast #-}


-- | Push cast over an argument to a function into that function
--
-- This is done by specializing on the casted argument.
-- Example:
-- @
--   y = f (cast a)
--     where f x = g x
-- @
-- transforms to:
-- @
--   y = f' a
--     where f' x' = (\x -> g x) (cast x')
-- @
--
-- The reason d'etre for this transformation is that we hope to end up with
-- and expression where two casts are "back-to-back" after which we can
-- eliminate them in 'eliminateCastCast'.
argCastSpec :: HasCallStack => NormRewrite
argCastSpec ctx e@(App f (stripTicks -> Cast e' _ _))
 -- Don't specialise when the arguments are casts-of-casts, these casts-of-casts
 -- will be eliminated by 'eliminateCastCast' during the normalization of the
 -- "current" function. We thus prevent the unnecessary introduction of a
 -- specialized version of 'f'.
 | not (isCast e)
 -- Don't specialise prims, because we can't push casts into them
 , not . isPrim . fst . collectArgs $ f = do
  bndrs <- Lens.use bindings
  isWorkFree workFreeBinders bndrs e' >>= \case
    True -> go
    False -> warn go
 where
  go = specializeNorm ctx e
  warn = trace (unwords
    [ "WARNING:", $(curLoc), "specializing a function on a non work-free"
    , "cast. Generated HDL implementation might contain duplicate work."
    , "Please report this as a bug.", "\n\nExpression where this occured:"
    , "\n\n" ++ showPpr e
    ])
argCastSpec _ e = return e
{-# SCC argCastSpec #-}

-- | Only inline casts that just contain a 'Var', because these are guaranteed work-free.
-- These are the result of the 'splitCastWork' transformation.
inlineCast :: HasCallStack => NormRewrite
inlineCast = inlineBinders test
  where
    test _ (_, (Cast (stripTicks -> Var {}) _ _)) = return True
    test _ _ = return False
{-# SCC inlineCast #-}

-- | Eliminate two back to back casts where the type going in and coming out are the same
--
-- @
--   (cast :: b -> a) $ (cast :: a -> b) x   ==> x
-- @
eliminateCastCast :: HasCallStack => NormRewrite
eliminateCastCast _ c@(Cast (stripTicks -> Cast e tyA tyB) tyB' tyC) = do
  tcm <- Lens.view tcCache
  let ntyA  = normalizeType tcm tyA
      ntyB  = normalizeType tcm tyB
      ntyB' = normalizeType tcm tyB'
      ntyC  = normalizeType tcm tyC
  if ntyB == ntyB' && ntyA == ntyC then changed e
                                   else throwError
  where throwError = do
          (nm,sp) <- Lens.use curFun
          throw (ClashException sp ($(curLoc) ++ showPpr nm
                  ++ ": Found 2 nested casts whose types don't line up:\n"
                  ++ showPpr c)
                Nothing)

eliminateCastCast _ e = return e
{-# SCC eliminateCastCast #-}

-- | Make a cast work-free by splitting the work of to a separate binding
--
-- @
-- let x = cast (f a b)
-- ==>
-- let x  = cast x'
--     x' = f a b
-- @
splitCastWork :: HasCallStack => NormRewrite
splitCastWork ctx@(TransformContext is0 _) unchanged@(Letrec vs e') = do
  (vss', Monoid.getAny -> hasChanged) <- listen (mapM (splitCastLetBinding is0) vs)
  let vs' = concat vss'
  if hasChanged then changed (Letrec vs' e')
                else return unchanged
  where
    splitCastLetBinding
      :: InScopeSet
      -> LetBinding
      -> RewriteMonad extra [LetBinding]
    splitCastLetBinding isN x@(nm, e) = case stripTicks e of
      Cast (Var {}) _ _  -> return [x]  -- already work-free
      Cast (Cast {}) _ _ -> return [x]  -- casts will be eliminated
      Cast e0 ty1 ty2 -> do
        tcm <- Lens.view tcCache
        nm' <- mkTmBinderFor isN tcm (mkDerivedName ctx (nameOcc $ varName nm)) e0
        changed [(nm',e0)
                ,(nm, Cast (Var nm') ty1 ty2)
                ]
      _ -> return [x]

splitCastWork _ e = return e
{-# SCC splitCastWork #-}


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

-- | Specialise functions on arguments which are constant, except when they
-- are clock, reset generators.
constantSpec :: HasCallStack => NormRewrite
constantSpec ctx@(TransformContext is0 tfCtx) e@(App e1 e2)
  | (Var {}, args) <- collectArgs e1
  , (_, []) <- Either.partitionEithers args
  , null $ Lens.toListOf termFreeTyVars e2
  = do specInfo<- constantSpecInfo ctx e2
       if csrFoundConstant specInfo then
         let newBindings = csrNewBindings specInfo in
         if null newBindings then
           -- Whole of e2 is constant
           specializeNorm ctx (App e1 e2)
         else do
           -- Parts of e2 are constant
           let is1 = extendInScopeSetList is0 (fst <$> csrNewBindings specInfo)
           Letrec newBindings
            <$> specializeNorm
                  (TransformContext is1 tfCtx)
                  (App e1 (csrNewTerm specInfo))

       else
        -- e2 has no constant parts
        return e
constantSpec _ e = return e
{-# SCC constantSpec #-}


-- Experimental

-- | Propagate arguments of application inwards; except for 'Lam' where the
-- argument becomes let-bound. 'appPropFast' tries to propagate as many arguments
-- as possible, down as many levels as possible; and should be called in a
-- top-down traversal.
--
-- The idea is that this reduces the number of traversals, which hopefully leads
-- to shorter compile times.
--
-- Note [AppProp no shadowing]
--
-- Case 1.
--
-- Imagine:
--
-- @
-- (case x of
--    D a b -> h a) (f x y)
-- @
--
-- rewriting this to:
--
-- @
-- let b = f x y
-- in  case x of
--       D a b -> h a b
-- @
--
-- is very bad because 'b' in 'h a b' is now bound by the pattern instead of the
-- newly introduced let-binding
--
-- instead me must deshadow w.r.t. the new variable and rewrite to:
--
-- @
-- let b = f x y
-- in  case x of
--       D a b1 -> h a b
-- @
--
-- Case 2.
--
-- Imagine
--
-- @
-- (\x -> e) u
-- @
--
-- where @u@ has a free variable named @x@, rewriting this to:
--
-- @
-- let x = u
-- in  e
-- @
--
-- would be very bad, because the let-binding suddenly captures the free
-- variable in @u@. To prevent this from happening we over-approximate and check
-- whether @x@ is in the current InScopeSet, and deshadow if that's the case,
-- i.e. we then rewrite to:
--
-- let x1 = u
-- in  e [x:=x1]
--
-- Case 3.
--
-- The same for:
--
-- @
-- (let x = w in e) u
-- @
--
-- where @u@ again has a free variable @x@, rewriting this to:
--
-- @
-- let x = w in (e u)
-- @
--
-- would be bad because the let-binding now captures the free variable in @u@.
--
-- To prevent this from happening, we unconditionally deshadow the function part
-- of the application w.r.t. the free variables in the argument part of the
-- application. It is okay to over-approximate in this case and deshadow w.r.t
-- the current InScopeSet.
appPropFast :: HasCallStack => NormRewrite
appPropFast ctx@(TransformContext is _) = \case
  e@App {}
    | let (fun,args,ticks) = collectArgsTicks e
    -> go is (deShadowTerm is fun) args ticks
  e@TyApp {}
    | let (fun,args,ticks) = collectArgsTicks e
    -> go is (deShadowTerm is fun) args ticks
  e          -> return e
 where
  go :: InScopeSet -> Term -> [Either Term Type] -> [TickInfo]
     -> NormalizeSession Term
  go is0 (collectArgsTicks -> (fun,args0@(_:_),ticks0)) args1 ticks1 =
    go is0 fun (args0 ++ args1) (ticks0 ++ ticks1)

  go is0 (Lam v e) (Left arg:args) ticks = do
    setChanged
    bndrs <- Lens.use bindings
    orM [pure (isVar arg), isWorkFree workFreeBinders bndrs arg] >>= \case
      True ->
        let subst = extendIdSubst (mkSubst is0) v arg in
        (`mkTicks` ticks) <$> go is0 (substTm "appPropFast.AppLam" subst e) args []
      False ->
        let is1 = extendInScopeSet is0 v in
        Letrec [(v, arg)] <$> go is1 (deShadowTerm is1 e) args ticks

  go is0 (Letrec vs e) args@(_:_) ticks = do
    setChanged
    let vbs  = map fst vs
        is1  = extendInScopeSetList is0 vbs
    -- XXX: 'vs' should already be deshadowed w.r.t. 'is0'
    Letrec vs <$> go is1 e args ticks

  go is0 (TyLam tv e) (Right t:args) ticks = do
    setChanged
    let subst = extendTvSubst (mkSubst is0) tv t
    (`mkTicks` ticks) <$> go is0 (substTm "appPropFast.TyAppTyLam" subst e) args []

  go is0 (Case scrut ty0 alts) args0@(_:_) ticks = do
    setChanged
    let isA1 = unionInScope
                 is0
                 ((mkInScopeSet . mkVarSet . concatMap (patVars . fst)) alts)
    (ty1,vs,args1) <- goCaseArg isA1 ty0 [] args0
    case vs of
      [] -> (`mkTicks` ticks) . Case scrut ty1 <$> mapM (goAlt is0 args1) alts
      _  -> do
        let vbs   = map fst vs
            is1   = extendInScopeSetList is0 vbs
            alts1 = map (deShadowAlt is1) alts
        Letrec vs . (`mkTicks` ticks) . Case scrut ty1 <$> mapM (goAlt is1 args1) alts1

  go is0 (Tick sp e) args ticks = do
    setChanged
    go is0 e args (sp:ticks)

  go _ fun args ticks = return (mkApps (mkTicks fun ticks) args)

  goAlt is0 args0 (p,e) = do
    let (tvs,ids) = patIds p
        is1       = extendInScopeSetList (extendInScopeSetList is0 tvs) ids
    (p,) <$> go is1 e args0 []

  goCaseArg isA ty0 ls0 (Right t:args0) = do
    tcm <- Lens.view tcCache
    let ty1 = piResultTy tcm ty0 t
    (ty2,ls1,args1) <- goCaseArg isA ty1 ls0 args0
    return (ty2,ls1,Right t:args1)

  goCaseArg isA0 ty0 ls0 (Left arg:args0) = do
    tcm <- Lens.view tcCache
    bndrs <- Lens.use bindings
    let argTy = termType tcm arg
        ty1   = applyFunTy tcm ty0 argTy
    orM [pure (isVar arg), isWorkFree workFreeBinders bndrs arg] >>= \case
      True -> do
        (ty2,ls1,args1) <- goCaseArg isA0 ty1 ls0 args0
        return (ty2,ls1,Left arg:args1)
      False -> do
        boundArg <- mkTmBinderFor isA0 tcm (mkDerivedName ctx "app_arg") arg
        let isA1 = extendInScopeSet isA0 boundArg
        (ty2,ls1,args1) <- goCaseArg isA1 ty1 ls0 args0
        return (ty2,(boundArg,arg):ls1,Left (Var boundArg):args1)

  goCaseArg _ ty ls [] = return (ty,ls,[])
{-# SCC appPropFast #-}

type NormRewriteW = Transform (StateT ([LetBinding],InScopeSet) (RewriteMonad NormalizeState))

-- | See Note [ANF InScopeSet]
tellBinders :: Monad m => [LetBinding] -> StateT ([LetBinding],InScopeSet) m ()
tellBinders bs = modify ((bs ++) *** (`extendInScopeSetList` (map fst bs)))

-- | See Note [ANF InScopeSet]; only extends the inscopeset
notifyBinders :: Monad m => [LetBinding] -> StateT ([LetBinding],InScopeSet) m ()
notifyBinders bs = modify (second (`extendInScopeSetList` (map fst bs)))

-- | Is the given type IO-like
isSimIOTy
  :: TyConMap
  -> Type
  -- ^ Type to check for IO-likeness
  -> Bool
isSimIOTy tcm ty = case tyView (coreView tcm ty) of
  TyConApp tcNm args
    | nameOcc tcNm == "Clash.Explicit.SimIO.SimIO"
    -> True
    | nameOcc tcNm == "GHC.Prim.(#,#)"
    , [_,_,st,_] <- args
    -> isStateTokenTy tcm st
  FunTy _ res -> isSimIOTy tcm res
  _ -> False

-- | Is the given type the state token
isStateTokenTy
  :: TyConMap
  -> Type
  -- ^ Type to check for state tokenness
  -> Bool
isStateTokenTy tcm ty = case tyView (coreView tcm ty) of
  TyConApp tcNm _ -> nameOcc tcNm == "GHC.Prim.State#"
  _ -> False

-- | Turn an expression into a modified ANF-form. As opposed to standard ANF,
-- constants do not become let-bound.
makeANF :: HasCallStack => NormRewrite
makeANF (TransformContext is0 ctx) (Lam bndr e) = do
  e' <- makeANF (TransformContext (extendInScopeSet is0 bndr)
                                  (LamBody bndr:ctx))
                e
  return (Lam bndr e')

makeANF _ e@(TyLam {}) = return e

makeANF ctx@(TransformContext is0 _) e0
  = do
    -- We need to freshen all binders in `e` because we're shuffling them around
    -- into a single let-binder, because even when binders don't shadow, they
    -- don't have to be unique within an expression. And so lifting them all
    -- to a single let-binder will cause issues when they're not unique.
    --
    -- We cannot make freshening part of collectANF, because when we generate
    -- new binders, we need to make sure those names do not conflict with _any_
    -- of the existing binders in the expression.
    --
    -- See also Note [ANF InScopeSet]
    let (is2,e1) = freshenTm is0 e0
    ((e2,(bndrs,_)),Monoid.getAny -> hasChanged) <-
      listen (runStateT (bottomupR collectANF ctx e1) ([],is2))
    case bndrs of
      [] -> if hasChanged then return e2 else return e0
      _  -> do
        let (e3,ticks) = collectTicks e2
            (srcTicks,nmTicks) = partitionTicks ticks
        -- Ensure that `AppendName` ticks still scope over the entire expression
        changed (mkTicks (Letrec bndrs (mkTicks e3 srcTicks)) nmTicks)
{-# SCC makeANF #-}

-- | Note [ANF InScopeSet]
--
-- The InScopeSet contains:
--
--    1. All the free variables of the expression we are traversing
--
--    2. All the bound variables of the expression we are traversing
--
--    3. The newly created let-bindings as we recurse back up the traversal
--
-- All of these are needed to created let-bindings that
--
--    * Do not shadow
--    * Are not shadowed
--    * Nor conflict with each other (i.e. have the same unique)
--
-- Initially we start with the local InScopeSet and add the global variables:
--
-- @
-- is1 <- unionInScope is0 <$> Lens.use globalInScope
-- @
--
-- Which will gives us the (superset of) free variables of the expression. Then
-- we call  'freshenTm'
--
-- @
-- let (is2,e1) = freshenTm is1 e0
-- @
--
-- Which extends the InScopeSet with all the bound variables in 'e1', the
-- version of 'e0' where all binders are unique (not just deshadowed).
--
-- So we start out with an InScopeSet that satisfies points 1 and 2, now every
-- time we create a new binder we must add it to the InScopeSet to satisfy
-- point 3.
--
-- Note [ANF no let-bind]
--
-- | Do not let-bind:
--
-- 1. Arguments with an untranslatable type: untranslatable expressions
--    should be propagated down as far as possible
--
-- 2. Local variables or constants: they don't add any work, so no reason
--    to let-bind to enable sharing
--
-- 3. IO actions, the translation of IO actions to sequential HDL constructs
--    depends on IO actions to be propagated down as far as possible.
collectANF :: HasCallStack => NormRewriteW
collectANF ctx e@(App appf arg)
  | (conVarPrim, _) <- collectArgs e
  , isCon conVarPrim || isPrim conVarPrim || isVar conVarPrim
  = do
    tcm <- Lens.view tcCache
    untranslatable <- lift (isUntranslatable False arg)
    let localVar   = isLocalVar arg
        constantNoCR = isConstantNotClockReset tcm arg
    -- See Note [ANF no let-bind]
    case (untranslatable,localVar || constantNoCR, isSimBind conVarPrim,arg) of
      (False,False,False,_) -> do
        -- See Note [ANF InScopeSet]
        is1   <- Lens.use _2
        argId <- lift (mkTmBinderFor is1 tcm (mkDerivedName ctx "app_arg") arg)
        -- See Note [ANF InScopeSet]
        tellBinders [(argId,arg)]
        return (App appf (Var argId))
      (True,False,_,Letrec binds body) -> do
        tellBinders binds
        return (App appf body)
      _ -> return e
 where
  isSimBind (Prim p) = primName p == "Clash.Explicit.SimIO.bindSimIO#"
  isSimBind _ = False

collectANF _ (Letrec binds body) = do
  tcm <- Lens.view tcCache
  let isSimIO = isSimIOTy tcm (termType tcm body)
  untranslatable <- lift (isUntranslatable False body)
  let localVar = isLocalVar body
  -- See Note [ANF no let-bind]
  if localVar || untranslatable || isSimIO
    then do
      tellBinders binds
      return body
    else do
      -- See Note [ANF InScopeSet]
      is1 <- Lens.use _2
      argId <- lift (mkTmBinderFor is1 tcm (mkUnsafeSystemName "result" 0) body)
      -- See Note [ANF InScopeSet]
      tellBinders [(argId,body)]
      tellBinders binds
      return (Var argId)

-- TODO: The code below special-cases ANF for the ':-' constructor for the
-- 'Signal' type. The 'Signal' type is essentially treated as a "transparent"
-- type by the Clash compiler, so observing its constructor leads to all kinds
-- of problems. In this case that "Clash.Rewrite.Util.mkSelectorCase" will
-- try to project the LHS and RHS of the ':-' constructor, however,
-- 'mkSelectorCase' uses 'coreView1' to find the "real" data-constructor.
-- 'coreView1' however looks through the 'Signal' type, and hence 'mkSelector'
-- finds the data constructors for the element type of Signal. This resulted in
-- error #24 (https://github.com/christiaanb/clash2/issues/24), where we
-- try to get the first field out of the 'Vec's 'Nil' constructor.
--
-- Ultimately we should stop treating Signal as a "transparent" type and deal
-- handling of the Signal type, and the involved co-recursive functions,
-- properly. At the moment, Clash cannot deal with this recursive type and the
-- recursive functions involved, hence the need for special-casing code. After
-- everything is done properly, we should remove the two lines below.
collectANF _ e@(Case _ _ [(DataPat dc _ _,_)])
  | nameOcc (dcName dc) == "Clash.Signal.Internal.:-" = return e

collectANF ctx (Case subj ty alts) = do
    let localVar = isLocalVar subj
    let isConstantSubj = isConstant subj

    (subj',subjBinders) <- if localVar || isConstantSubj
      then return (subj,[])
      else do
        tcm <- Lens.view tcCache
        -- See Note [ANF InScopeSet]
        is1 <- Lens.use _2
        argId <- lift (mkTmBinderFor is1 tcm (mkDerivedName ctx "case_scrut") subj)
        -- See Note [ANF InScopeSet]
        notifyBinders [(argId,subj)]
        return (Var argId,[(argId,subj)])

    tcm <- Lens.view tcCache
    let isSimIOAlt = isSimIOTy tcm ty

    alts' <- mapM (doAlt isSimIOAlt subj') alts
    tellBinders subjBinders

    case alts' of
      [(DataPat _ [] xs,altExpr)]
        | xs `localIdsDoNotOccurIn` altExpr || isSimIOAlt
        -> return altExpr
      _ -> return (Case subj' ty alts')
  where
    doAlt
      :: Bool -> Term -> (Pat,Term)
      -> StateT ([LetBinding],InScopeSet) (RewriteMonad NormalizeState)
                (Pat,Term)
    doAlt isSimIOAlt subj' alt@(DataPat dc exts xs,altExpr) | not (bindsExistentials exts xs) = do
      let lv = isLocalVar altExpr
      patSels <- Monad.zipWithM (doPatBndr subj' dc) xs [0..]
      let altExprIsConstant = isConstant altExpr
      let usesXs (Var n) = any (== n) xs
          usesXs _       = False
      -- See [ANF no let-bind]
      if or [isSimIOAlt, lv && (not (usesXs altExpr) || length alts == 1), altExprIsConstant]
        then do
          -- See Note [ANF InScopeSet]
          tellBinders patSels
          return alt
        else do
          tcm <- Lens.view tcCache
          -- See Note [ANF InScopeSet]
          is1 <- Lens.use _2
          altId <- lift (mkTmBinderFor is1 tcm (mkDerivedName ctx "case_alt") altExpr)
          -- See Note [ANF InScopeSet]
          tellBinders (patSels ++ [(altId,altExpr)])
          return (DataPat dc exts xs,Var altId)
    doAlt _ _ alt@(DataPat {}, _) = return alt
    doAlt isSimIOAlt _ alt@(pat,altExpr) = do
      let lv = isLocalVar altExpr
      let altExprIsConstant = isConstant altExpr
      -- See [ANF no let-bind]
      if isSimIOAlt || lv || altExprIsConstant
        then return alt
        else do
          tcm <- Lens.view tcCache
          -- See Note [ANF InScopeSet]
          is1 <- Lens.use _2
          altId <- lift (mkTmBinderFor is1 tcm (mkDerivedName ctx "case_alt") altExpr)
          tellBinders [(altId,altExpr)]
          return (pat,Var altId)

    doPatBndr
      :: Term -> DataCon -> Id -> Int
      -> StateT ([LetBinding],InScopeSet) (RewriteMonad NormalizeState)
                LetBinding
    doPatBndr subj' dc pId i
      = do
        tcm <- Lens.view tcCache
        -- See Note [ANF InScopeSet]
        is1 <- Lens.use _2
        patExpr <- lift (mkSelectorCase ($(curLoc) ++ "doPatBndr") is1 tcm subj' (dcTag dc) i)
        -- No need to 'tellBinders' here because 'pId' is already in the ANF
        -- InScopeSet.
        --
        -- See also Note [ANF InScopeSet]
        return (pId,patExpr)

collectANF _ e = return e
{-# SCC collectANF #-}

-- | Eta-expand top-level lambda's (DON'T use in a traversal!)
etaExpansionTL :: HasCallStack => NormRewrite
etaExpansionTL (TransformContext is0 ctx) (Lam bndr e) = do
  e' <- etaExpansionTL
          (TransformContext (extendInScopeSet is0 bndr) (LamBody bndr:ctx))
          e
  return $ Lam bndr e'

etaExpansionTL (TransformContext is0 ctx) (Letrec xes e) = do
  let bndrs = map fst xes
  e' <- etaExpansionTL
          (TransformContext (extendInScopeSetList is0 bndrs)
                            (LetBody bndrs:ctx))
          e
  case stripLambda e' of
    (bs@(_:_),e2) -> do
      let e3 = Letrec xes e2
      changed (mkLams e3 bs)
    _ -> return (Letrec xes e')
  where
    stripLambda :: Term -> ([Id],Term)
    stripLambda (Lam bndr e0) =
      let (bndrs,e1) = stripLambda e0
      in  (bndr:bndrs,e1)
    stripLambda e' = ([],e')

etaExpansionTL (TransformContext is0 ctx) e
  = do
    tcm <- Lens.view tcCache
    if isFun tcm e
      then do
        let argTy = ( fst
                    . Maybe.fromMaybe (error $ $(curLoc) ++ "etaExpansion splitFunTy")
                    . splitFunTy tcm
                    . termType tcm
                    ) e
        newId <- mkInternalVar is0 "arg" argTy
        e' <- etaExpansionTL (TransformContext (extendInScopeSet is0 newId)
                                               (LamBody newId:ctx))
                             (App e (Var newId))
        changed (Lam newId e')
      else return e
{-# SCC etaExpansionTL #-}

-- | Eta-expand functions with a Synthesize annotation, needed to allow such
-- functions to appear as arguments to higher-order primitives.
etaExpandSyn :: HasCallStack => NormRewrite
etaExpandSyn (TransformContext is0 ctx) e@(collectArgs -> (Var f, _)) = do
  topEnts <- Lens.view topEntities
  tcm <- Lens.view tcCache
  let isTopEnt = f `elemVarSet` topEnts
      isAppFunCtx =
        \case
          AppFun:_ -> True
          TickC _:c -> isAppFunCtx c
          _ -> False
      argTyM = fmap fst (splitFunTy tcm (termType tcm e))
  case argTyM of
    Just argTy | isTopEnt && not (isAppFunCtx ctx) -> do
      newId <- mkInternalVar is0 "arg" argTy
      changed (Lam newId (App e (Var newId)))
    _ -> return e

etaExpandSyn _ e = return e
{-# SCC etaExpandSyn #-}


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
                  lvl <- Lens.view dbgLevel
                  traceIf (lvl > DebugNone) ($(curLoc) ++ "InlineHO: " ++ show f ++ " already inlined " ++ show limit ++ " times in:" ++ show cf) (return e)
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
            | TyConApp (nameOcc -> "Clash.Explicit.SimIO.SimIO") _ <- tyView aTy
            -> True
          _ -> False
      | id_ `notElemVarSet` bodyFVs
      = case tm of
          Prim pInfo
            | primName pInfo `elem` [ "Clash.Explicit.SimIO.openFile"
                        , "Clash.Explicit.SimIO.fgetc"
                        , "Clash.Explicit.SimIO.feof"
                        ]
            , Just occ <- lookupVarEnv id_ allOccs
            , occ < 2
            -> True
            | otherwise
            -> primName pInfo `elem` ["Clash.Explicit.SimIO.bindSimIO#"]
          Case _ _ [(DataPat dcE _ _,_)]
            -> let nm = (nameOcc (dcName dcE))
               in -- Inlines WW projection that exposes internals of the BitVector types
                  nm == "Clash.Sized.Internal.BitVector.BV"  ||
                  nm == "Clash.Sized.Internal.BitVector.Bit" ||
                  -- Inlines projections out of constraint-tuples (e.g. HiddenClockReset)
                  "GHC.Classes" `Text.isPrefixOf` nm
          Case _ aTy (_:_:_)
            | TyConApp (nameOcc -> "Clash.Explicit.SimIO.SimIO") _ <- tyView aTy
            -> True
          _ -> False

    isInteresting _ _ _ _ = False

inlineCleanup _ e = return e
{-# SCC inlineCleanup #-}

-- | Mark to track progress of 'reduceBindersCleanup'
data Mark = Temp | Done | Rec

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

-- | Worker function of 'separateArguments'.
separateLambda
  :: TyConMap
  -> TransformContext
  -> Id
  -- ^ Lambda binder
  -> Term
  -- ^ Lambda body
  -> Maybe Term
  -- ^ If lambda is split up, this function returns a Just containing the new term
separateLambda tcm ctx@(TransformContext is0 _) b eb0 =
  case shouldSplit tcm (varType b) of
    Just (dc, _, argTys) ->
      let
        nm = mkDerivedName ctx (nameOcc (varName b))
        bs0 = map (`mkLocalId` nm) argTys
        (is1, bs1) = List.mapAccumL newBinder is0 bs0
        subst = extendIdSubst (mkSubst is1) b (dc (map Var bs1))
        eb1 = substTm "separateArguments" subst eb0
      in
        Just (mkLams eb1 bs1)
    _ ->
      Nothing
 where
  newBinder isN0 x =
    let
      x' = uniqAway isN0 x
      isN1 = extendInScopeSet isN0 x'
    in
      (isN1, x')
{-# SCC separateLambda #-}

-- | Split apart (global) function arguments that contain types that we
-- want to separate off, e.g. Clocks. Works on both the definition side (i.e. the
-- lambda), and the call site (i.e. the application of the global variable). e.g.
-- turns
--
-- > f :: (Clock System, Reset System) -> Signal System Int
--
-- into
--
-- > f :: Clock System -> Reset System -> Signal System Int
separateArguments :: HasCallStack => NormRewrite
separateArguments ctx e0@(Lam b eb) = do
  tcm <- Lens.view tcCache
  case separateLambda tcm ctx b eb of
    Just e1 -> changed e1
    Nothing -> return e0

separateArguments (TransformContext is0 _) e@(collectArgsTicks -> (Var g, args, ticks))
  | isGlobalId g = do
  -- We ensure that both the type of the global variable reference is updated
  -- to take into account the changed arguments, and that we apply the global
  -- function with the split apart arguments.
  let (argTys0,resTy) = splitFunForallTy (varType g)
  (concat -> args1, Monoid.getAny -> hasChanged)
    <- listen (mapM (uncurry splitArg) (zip argTys0 args))
  if hasChanged then
    let (argTys1,args2) = unzip args1
        gTy = mkPolyFunTy resTy argTys1
    in  return (mkApps (mkTicks (Var g {varType = gTy}) ticks) args2)
  else
    return e

 where
  -- Split a single argument
  splitArg
    :: Either TyVar Type
    -- The quantifier/function argument type of the global variable
    -> Either Term Type
    -- The applied type argument or term argument
    -> NormalizeSession [(Either TyVar Type,Either Term Type)]
  splitArg tv arg@(Right _)    = return [(tv,arg)]
  splitArg ty arg@(Left tmArg) = do
    tcm <- Lens.view tcCache
    let argTy = termType tcm tmArg
    case shouldSplit tcm argTy of
      Just (_,Projections projections,_) -> do
        tmArgs <- projections is0 tmArg
        changed (map ((ty,) . Left) tmArgs)
      _ ->
        return [(ty,arg)]

separateArguments _ e = return e
{-# SCC separateArguments #-}

-- | Remove all undefined alternatives from case expressions, replacing them
-- with the value of another defined alternative. If there is one defined
-- alternative, the entire expression is replaced with that alternative. If
-- there are no defined alternatives, the entire expression is replaced with
-- a call to 'errorX'.
--
-- e.g. It converts
--
--     case x of
--       D1 a -> f a
--       D2   -> undefined
--       D3   -> undefined
--
-- to
--
--     let subj = x
--         a    = case subj of
--                  D1 a -> field0
--      in f a
--
-- where fieldN is an internal variable referring to the nth argument of a
-- data constructor.
--
xOptimize :: HasCallStack => NormRewrite
xOptimize (TransformContext is0 _) e@(Case subj ty alts) = do
  runXOpt <- Lens.view aggressiveXOpt

  if runXOpt then do
    defPart <- List.partitionM (isPrimError . snd) alts

    case defPart of
      ([], _)    -> return e
      (_, [])    -> changed (Prim (PrimInfo "Clash.XException.errorX" ty WorkConstant SingleResult))
      (_, [alt]) -> xOptimizeSingle is0 subj alt
      (_, defs)  -> xOptimizeMany is0 subj ty defs
  else
    return e

xOptimize _ e = return e
{-# SCC xOptimize #-}

-- Return an expression equivalent to the alternative given. When only one
-- alternative is defined the result of this function is used to replace the
-- case expression.
--
xOptimizeSingle :: InScopeSet -> Term -> Alt -> NormalizeSession Term
xOptimizeSingle is subj (DataPat dc tvs vars, expr) = do
  tcm    <- Lens.view tcCache
  subjId <- mkInternalVar is "subj" (termType tcm subj)

  let fieldTys = fmap varType vars
  lets <- Monad.zipWithM (mkFieldSelector is subjId dc tvs fieldTys) vars [0..]

  changed (Letrec ((subjId, subj) : lets) expr)

xOptimizeSingle _ _ (_, expr) = changed expr

-- Given a list of alternatives which are defined, create a new case
-- expression which only ever returns a defined value.
--
xOptimizeMany
  :: HasCallStack
  => InScopeSet
  -> Term
  -> Type
  -> [Alt]
  -> NormalizeSession Term
xOptimizeMany is subj ty defs@(d:ds)
  | isAnyDefault defs = changed (Case subj ty defs)
  | otherwise = do
      newAlt <- xOptimizeSingle is subj d
      changed (Case subj ty $ ds <> [(DefaultPat, newAlt)])
 where
  isAnyDefault :: [Alt] -> Bool
  isAnyDefault = any ((== DefaultPat) . fst)

xOptimizeMany _ _ _ [] =
  error $ $(curLoc) ++ "Report as bug: xOptimizeMany error: No defined alternatives"

mkFieldSelector
  :: MonadUnique m
  => InScopeSet
  -> Id
  -- ^ subject id
  -> DataCon
  -> [TyVar]
  -> [Type]
  -- ^ concrete types of fields
  -> Id
  -> Int
  -> m LetBinding
mkFieldSelector is0 subj dc tvs fieldTys nm index = do
  fields <- mapM (\ty -> mkInternalVar is0 "field" ty) fieldTys
  let alt = (DataPat dc tvs fields, Var $ fields !! index)
  return (nm, Case (Var subj) (fieldTys !! index) [alt])

-- Check whether a term is really a black box primitive representing an error.
-- Such values are undefined and are removed in X Optimization.
--
isPrimError :: Term -> NormalizeSession Bool
isPrimError (collectArgs -> (Prim pInfo, _)) = do
  prim <- Lens.use (extra . primitives . Lens.at (primName pInfo))

  case prim >>= extractPrim of
    Just p  -> return (isErr p)
    Nothing -> return False
 where
  isErr BlackBox{template=(BBTemplate [Err _])} = True
  isErr _ = False

isPrimError _ = return False

-- A multi result primitive assigns its results to multiple result variables
-- instead of one. Besides producing nicer HDL it works around issues with
-- synthesis tooling described in:
--
--   https://github.com/clash-lang/clash-compiler/issues/1555
--
-- This transformation rewrites primitives indicating they can assign their
-- results to multiple signals, such that netlist can easily render it.
--
-- Example:
--
-- @
-- prim :: forall a. a -> (a, a)
-- @
--
-- will be rewritten to:
--
-- @
--   \a0 -> let
--            r  = prim @t0 a0 r0 r1     -- With 'Clash.Core.Term.MultiPrim'
--            r0 = multiPrimSelect r0 r
--            r1 = multiPrimSelect r1 r
--          in
--            (x, y)
-- @
--
-- Netlist will not render any @multiPrimSelect@ primitives. Similar to
-- primitives having a /void/ return type, /r/ is not rendered either.
--
-- This transformation is currently hardcoded to recognize tuples as return
-- types, not any product type. It will error if it sees a multi result primitive
-- with a non-tuple return type.
--
setupMultiResultPrim :: HasCallStack => NormRewrite
setupMultiResultPrim _ctx e@(Prim pInfo@PrimInfo{primMultiResult=SingleResult}) = do
  tcm <- Lens.view tcCache
  prim <- Lens.use (extra . primitives . Lens.at (primName pInfo))

  case prim >>= extractPrim of
    Just (BlackBoxHaskell{multiResult=True}) ->
      changed (setupMultiResultPrim' tcm pInfo)
    Just (BlackBox{multiResult=True}) ->
      changed (setupMultiResultPrim' tcm pInfo)
    _ ->
      return e

setupMultiResultPrim _ e = return e

setupMultiResultPrim' :: HasCallStack => TyConMap -> PrimInfo -> Term
setupMultiResultPrim' tcm primInfo@PrimInfo{primType} =
  mkAbstraction letTerm (map Right typeVars <> map Left argIds)
 where
  typeVars = Either.lefts pArgs

  internalNm prefix n = mkUnsafeInternalName (prefix <> showt n) n
  internalId prefix typ n = mkLocalId typ (internalNm prefix n)

  nTermArgs = length (Either.rights pArgs)
  argIds = zipWith (internalId "a") (Either.rights pArgs) [1..nTermArgs]
  resIds = zipWith (internalId "r") resTypes [nTermArgs+1..nTermArgs+length resTypes]
  resId = mkLocalId pResTy (mkUnsafeInternalName "r" (nTermArgs+length resTypes+1))

  (pArgs, pResTy) = splitFunForallTy primType
  MultiPrimInfo{mpi_resultDc=tupTc, mpi_resultTypes=resTypes} =
    multiPrimInfo' tcm primInfo

  multiPrimSelect r t = (r, mkTmApps (Prim (multiPrimSelectInfo t)) [Var r, Var resId])
  multiPrimSelectBinds = zipWith multiPrimSelect  resIds resTypes
  multiPrimTermArgs = map (Left . Var) (argIds <> resIds)
  multiPrimTypeArgs = map (Right . VarTy) typeVars
  multiPrimBind =
    mkApps
      (Prim primInfo{primMultiResult=MultiResult})
      (multiPrimTypeArgs <> multiPrimTermArgs)

  multiPrimSelectInfo t = PrimInfo
    { primName = "c$multiPrimSelect"
    , primType = mkPolyFunTy pResTy [Right pResTy, Right t]
    , primWorkInfo = WorkAlways
    , primMultiResult = SingleResult }

  letTerm =
    Letrec
      ((resId,multiPrimBind):multiPrimSelectBinds)
      (mkTmApps (mkTyApps (Data tupTc) resTypes) (map Var resIds))

-- | Inline anything of type `SimIO`: IO actions cannot be shared
inlineSimIO :: HasCallStack => NormRewrite
inlineSimIO = inlineBinders test
  where
    test _ (i,_) = case tyView (varType i) of
      TyConApp tc _ -> return $! nameOcc tc == "Clash.Explicit.SimIO.SimIO"
      _ -> return False
{-# SCC inlineSimIO #-}
