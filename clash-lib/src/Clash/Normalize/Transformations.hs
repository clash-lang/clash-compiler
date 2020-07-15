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
  ( caseLet
  , caseCon
  , caseCase
  , caseElemNonReachable
  , elemExistentials
  , inlineNonRep
  , inlineOrLiftNonRep
  , typeSpec
  , nonRepSpec
  , etaExpansionTL
  , nonRepANF
  , bindConstantVar
  , constantSpec
  , makeANF
  , deadCode
  , topLet
  , recToLetRec
  , inlineWorkFree
  , inlineHO
  , inlineSmall
  , simpleCSE
  , reduceConst
  , reduceNonRepPrim
  , caseFlat
  , disjointExpressionConsolidation
  , removeUnusedExpr
  , inlineCleanup
  , inlineBndrsCleanup
  , flattenLet
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
  )
where

import           Control.Exception           (throw)
import           Control.Lens                ((^.),_1,_2)
import qualified Control.Lens                as Lens
import qualified Control.Monad               as Monad
import           Control.Monad.Extra         (orM)
import           Control.Monad.State         (StateT (..), modify)
import           Control.Monad.State.Strict  (evalState)
import           Control.Monad.Writer        (lift, listen)
import           Control.Monad.Trans.Except  (runExcept)
import           Data.Coerce                 (coerce)
import qualified Data.Either                 as Either
import qualified Data.HashMap.Lazy           as HashMap
import qualified Data.HashMap.Strict         as HashMapS
import           Data.List                   ((\\))
import qualified Data.List                   as List
import qualified Data.List.Extra             as List
import qualified Data.Maybe                  as Maybe
import qualified Data.Monoid                 as Monoid
import qualified Data.Primitive.ByteArray    as BA
import qualified Data.Text                   as Text
import           GHC.Integer.GMP.Internals   (Integer (..), BigNat (..))

import           BasicTypes                  (InlineSpec (..))

import           Clash.Annotations.Primitive (extractPrim)
import           Clash.Core.DataCon          (DataCon (..))
import           Clash.Core.EqSolver
import           Clash.Core.Name
  (Name (..), NameSort (..), mkUnsafeSystemName, nameOcc)
import           Clash.Core.FreeVars
  (localIdOccursIn, localIdsDoNotOccurIn, freeLocalIds, termFreeTyVars,
   typeFreeVars, localVarsDoNotOccurIn, localIdDoesNotOccurIn,
   countFreeOccurances)
import           Clash.Core.Literal          (Literal (..))
import           Clash.Core.Pretty           (showPpr)
import           Clash.Core.Subst
import           Clash.Core.Term
import           Clash.Core.TermInfo
import           Clash.Core.Type             (Type (..), TypeView (..), applyFunTy,
                                              isPolyFunCoreTy, isClassTy,
                                              normalizeType, splitFunForallTy,
                                              splitFunTy,
                                              tyView, mkPolyFunTy, coreView,
                                              LitTy (..), coreView1)
import           Clash.Core.TyCon            (TyConMap, tyConDataCons)
import           Clash.Core.Util
  ( isSignalType, mkVec, tyNatSize, undefinedTm,
   shouldSplit, inverseTopSortLetBindings)
import           Clash.Core.Var
  (Id, TyVar, Var (..), isGlobalId, isLocalId, mkLocalId)
import           Clash.Core.VarEnv
  (InScopeSet, VarEnv, VarSet, elemVarSet,
   emptyVarEnv, extendInScopeSet, extendInScopeSetList, lookupVarEnv,
   notElemVarSet, unionVarEnvWith, unionInScope, unitVarEnv,
   unitVarSet, mkVarSet, mkInScopeSet, uniqAway, elemInScopeSet, elemVarEnv,
   foldlWithUniqueVarEnv', lookupVarEnvDirectly, extendVarEnv, unionVarEnv,
   eltsVarEnv, mkVarEnv, elemUniqInScopeSet)
import           Clash.Debug
import           Clash.Driver.Types          (Binding(..), DebugLevel (..))
import           Clash.Netlist.BlackBox.Types (Element(Err))
import           Clash.Netlist.BlackBox.Util (getUsedArguments)
import           Clash.Netlist.Types         (BlackBox(..), HWType (..), FilteredHWType(..))
import           Clash.Netlist.Util
  (coreTypeToHWType, representableType, splitNormalized, bindsExistentials)
import           Clash.Normalize.DEC
import           Clash.Normalize.PrimitiveReductions
import           Clash.Normalize.Types
import           Clash.Normalize.Util
import           Clash.Primitives.Types
  (Primitive(..), TemplateKind(TExpr), CompiledPrimMap, UsedArguments(..))
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

-- | Lift the let-bindings out of the subject of a Case-decomposition
caseLet :: HasCallStack => NormRewrite
caseLet (TransformContext is0 _) (Case (collectTicks -> (Letrec xes e,ticks)) ty alts) = do
  -- Note [CaseLet deshadow]
  -- Imagine
  --
  -- @
  -- case (let x = u in e) of {p -> a}
  -- @
  --
  -- where `a` has a free variable named `x`.
  --
  -- Simply transforming the above to:
  --
  -- @
  -- let x = u in case e of {p -> a}
  -- @
  --
  -- would be very bad, because now the let-binding captures the free x variable
  -- in a.
  --
  -- We must therefor rename `x` so that it doesn't capture the free variables
  -- in the alternative:
  --
  -- @
  -- let x1 = u[x:=x1] in case e[x:=x1] of {p -> a}
  -- @
  --
  -- It is safe to over-approximate the free variables in `a` by simply taking
  -- the current InScopeSet.
  let (xes1,e1) = deshadowLetExpr is0 xes e
  changed (Letrec (map (second (`mkTicks` ticks)) xes1)
                  (Case (mkTicks e1 ticks) ty alts))

caseLet _ e = return e
{-# SCC caseLet #-}

-- | Remove non-reachable alternatives. For example, consider:
--
--    data STy ty where
--      SInt :: Int -> STy Int
--      SBool :: Bool -> STy Bool
--
--    f :: STy ty -> ty
--    f (SInt b) = b + 1
--    f (SBool True) = False
--    f (SBool False) = True
--    {-# NOINLINE f #-}
--
--    g :: STy Int -> Int
--    g = f
--
-- @f@ is always specialized on @STy Int@. The SBool alternatives are therefore
-- unreachable. Additional information can be found at:
-- https://github.com/clash-lang/clash-compiler/pull/465
caseElemNonReachable :: HasCallStack => NormRewrite
caseElemNonReachable _ case0@(Case scrut altsTy alts0) = do
  tcm <- Lens.view tcCache

  let (altsAbsurd, altsOther) = List.partition (isAbsurdAlt tcm) alts0
  case altsAbsurd of
    [] -> return case0
    _  -> changed =<< caseOneAlt (Case scrut altsTy altsOther)

caseElemNonReachable _ e = return e
{-# SCC caseElemNonReachable #-}

-- | Tries to eliminate existentials by using heuristics to determine what the
-- existential should be. For example, consider Vec:
--
--    data Vec :: Nat -> Type -> Type where
--      Nil       :: Vec 0 a
--      Cons x xs :: a -> Vec n a -> Vec (n + 1) a
--
-- Thus, 'null' (annotated with existentials) could look like:
--
--    null :: forall n . Vec n Bool -> Bool
--    null v =
--      case v of
--        Nil  {n ~ 0}                                     -> True
--        Cons {n1:Nat} {n~n1+1} (x :: a) (xs :: Vec n1 a) -> False
--
-- When it's applied to a vector of length 5, this becomes:
--
--    null :: Vec 5 Bool -> Bool
--    null v =
--      case v of
--        Nil  {5 ~ 0}                                     -> True
--        Cons {n1:Nat} {5~n1+1} (x :: a) (xs :: Vec n1 a) -> False
--
-- This function solves 'n1' and replaces every occurrence with its solution. A
-- very limited number of solutions are currently recognized: only adds (such
-- as in the example) will be solved.
elemExistentials :: HasCallStack => NormRewrite
elemExistentials (TransformContext is0 _) (Case scrut altsTy alts0) = do
  tcm <- Lens.view tcCache

  alts1 <- mapM (go is0 tcm) alts0
  caseOneAlt (Case scrut altsTy alts1)

 where
    -- Eliminate free type variables if possible
    go :: InScopeSet -> TyConMap -> (Pat, Term) -> NormalizeSession (Pat, Term)
    go is2 tcm alt@(DataPat dc exts0 xs0, term0) =
      case solveNonAbsurds tcm (altEqs tcm alt) of
        -- No equations solved:
        [] -> return alt
        -- One or more equations solved:
        sols ->
          changed =<< go is2 tcm (DataPat dc exts1 xs1, term1)
          where
            -- Substitute solution in existentials and applied types
            is3   = extendInScopeSetList is2 exts0
            xs1   = map (substTyInVar (extendTvSubstList (mkSubst is3) sols)) xs0
            exts1 = substInExistentialsList is2 exts0 sols

            -- Substitute solution in term.
            is4       = extendInScopeSetList is3 xs1
            subst     = extendTvSubstList (mkSubst is4) sols
            term1     = substTm "Replacing tyVar due to solved eq" subst term0

    go _ _ alt = return alt

elemExistentials _ e = return e
{-# SCC elemExistentials #-}

-- | Move a Case-decomposition from the subject of a Case-decomposition to the alternatives
caseCase :: HasCallStack => NormRewrite
caseCase (TransformContext is0 _) e@(Case (stripTicks -> Case scrut alts1Ty alts1) alts2Ty alts2)
  = do
    ty1Rep <- representableType <$> Lens.view typeTranslator
                                <*> Lens.view customReprs
                                <*> pure False
                                <*> Lens.view tcCache
                                <*> pure alts1Ty
    if not ty1Rep
      -- Deshadow to prevent accidental capture of free variables of inner
      -- case. Imagine:
      --
      --   case (case a of {x -> x}) of {_ -> x}
      --
      -- 'x' is introduced the inner 'case' and used (as a free variable) in
      -- the outer one. The goal of 'caseCase' is to rewrite cases such that
      -- their subjects aren't cases. This is achieved by 'pushing' the outer
      -- case to all the alternatives of the inner one. Naively doing so in
      -- this example would cause an accidental capture:
      --
      --   case a of {x -> case x of {_ -> x}}
      --
      -- Suddenly, the 'x' in the alternative of the inner case statement
      -- refers to the one introduced by the outer one, instead of being a
      -- free variable. To prevent this, we deshadow the alternatives of the
      -- original inner case. We now end up with:
      --
      --   case a of {x1 -> case x1 of {_ -> x}}
      --
      then let newAlts = map
                           (second (\altE -> Case altE alts2Ty alts2))
                           (map (deShadowAlt is0) alts1)
           in  changed $ Case scrut alts2Ty newAlts
      else return e

caseCase _ e = return e
{-# SCC caseCase #-}

-- | Inline function with a non-representable result if it's the subject
-- of a Case-decomposition
inlineNonRep :: HasCallStack => NormRewrite
inlineNonRep _ e@(Case scrut altsTy alts)
  | (Var f, args,ticks) <- collectArgsTicks scrut
  , isGlobalId f
  = do
    (cf,_)    <- Lens.use curFun
    isInlined <- zoomExtra (alreadyInlined f cf)
    limit     <- Lens.use (extra.inlineLimit)
    tcm       <- Lens.view tcCache
    let scrutTy = termType tcm scrut
        noException = not (exception tcm scrutTy)
    if noException && (Maybe.fromMaybe 0 isInlined) > limit
      then
        trace (concat [ $(curLoc) ++ "InlineNonRep: " ++ showPpr (varName f)
                      ," already inlined " ++ show limit ++ " times in:"
                      , showPpr (varName cf)
                      , "\nType of the subject is: " ++ showPpr scrutTy
                      , "\nFunction " ++ showPpr (varName cf)
                      , " will not reach a normal form, and compilation"
                      , " might fail."
                      , "\nRun with '-fclash-inline-limit=N' to increase"
                      , " the inlining limit to N."
                      ])
              (return e)
      else do
        bodyMaybe   <- lookupVarEnv f <$> Lens.use bindings
        nonRepScrut <- not <$> (representableType <$> Lens.view typeTranslator
                                                  <*> Lens.view customReprs
                                                  <*> pure False
                                                  <*> Lens.view tcCache
                                                  <*> pure scrutTy)
        case (nonRepScrut, bodyMaybe) of
          (True,Just b) -> do
            Monad.when noException (zoomExtra (addNewInline f cf))

            let scrutBody0 = mkTicks (bindingTerm b) (mkInlineTick f : ticks)
            let scrutBody1 = mkApps scrutBody0 args

            changed $ Case scrutBody1 altsTy alts

          _ -> return e
  where
    exception = isClassTy

inlineNonRep _ e = return e
{-# SCC inlineNonRep #-}

-- | Specialize a Case-decomposition (replace by the RHS of an alternative) if
-- the subject is (an application of) a DataCon; or if there is only a single
-- alternative that doesn't reference variables bound by the pattern.
--
-- Note [CaseCon deshadow]
--
-- Imagine:
--
-- @
-- case D (f a b) (g x y) of
--   D a b -> h a
-- @
--
-- rewriting this to:
--
-- @
-- let a = f a b
-- in  h a
-- @
--
-- is very bad because the newly introduced let-binding now captures the free
-- variable 'a' in 'f a b'.
--
-- instead me must rewrite to:
--
-- @
-- let a1 = f a b
-- in  h a1
-- @
caseCon :: HasCallStack => NormRewrite
caseCon ctx@(TransformContext is0 _) e@(Case subj ty alts) = do
 tcm <- Lens.view tcCache
 case collectArgsTicks subj of
  -- The subject is an applied data constructor
  (Data dc, args, ticks) -> case List.find (equalCon . fst) alts of
    Just (DataPat _ tvs xs, altE) -> do
     let
      -- Create the substitution environment for all the existential
      -- type variables.
      exTysList = zip tvs (drop (length (dcUnivTyVars dc)) (Either.rights args))
      exTySubst = extendTvSubstList (mkSubst is0) exTysList
      -- Apply the type-substitution in all the pattern variables, we need
      -- to do this because we might use them as let-bindings later on,
      -- and they should have the correct type.
      xs1 = map (substTyInVar exTySubst) xs
      -- Create an initial set of let-binders for all variables used in the
      -- RHS of the alternative. We might later decide to substitute instead
      -- of let-bind in case the RHS of the let-binder is work-free.
      fvs = Lens.foldMapOf freeLocalIds unitVarSet altE
      (binds,_) = List.partition ((`elemVarSet` fvs) . fst)
                $ zip xs1 (Either.lefts args)
      binds1 = map (second (`mkTicks` ticks)) binds
     altE1 <-
       case binds1 of
        [] ->
          -- Apply the type-substitution for the existential type variables
          pure (substTm "caseCon1" exTySubst altE)
        _  -> do
          -- See Note [CaseCon deshadow]
          let
            -- Only let-bind expression that perform work.
            is1 = extendInScopeSetList (extendInScopeSetList is0 tvs) xs1
          ((is3,substIds),binds2) <- List.mapAccumLM newBinder (is1,[]) binds1
          let
            -- Create a substitution for all the existential type variables
            -- and the work-free expressions
            subst = extendIdSubstList
                      (extendTvSubstList (mkSubst is3) exTysList)
                      substIds
            body  = substTm "caseCon1" subst altE
          case Maybe.catMaybes binds2 of
            []     -> pure body
            binds3 -> pure (Letrec binds3 body)
     changed altE1
    _ -> case alts of
           -- In Core, default patterns always come first, so we match against
           -- that if there is one, and we couldn't match with any of the data
           -- patterns.
           ((DefaultPat,altE):_) -> changed altE
           _ -> changed (undefinedTm ty)
    where
      -- Check whether the pattern matches the data constructor
      equalCon (DataPat dcPat _ _) = dcTag dc == dcTag dcPat
      equalCon _                   = False

      -- Decide whether the applied arguments of the data constructor should
      -- be let-bound, or substituted into the alternative. We decide this
      -- based on the fact on whether the argument has the potential to make
      -- the circuit larger than needed if we were to duplicate that argument.
      newBinder (isN0, substN) (x, arg) = do
        isWorkFree arg >>= \case
          True -> pure ((isN0, (x, arg):substN), Nothing)
          False ->
            let
              x' = uniqAway isN0 x
              isN1 = extendInScopeSet isN0 x'
            in
              pure ((isN1, (x, Var x'):substN), Just (x', arg))


  -- The subject is a literal
  (Literal l,_,_) -> case List.find (equalLit . fst) alts of
    Just (LitPat _,altE) -> changed altE
    _ -> matchLiteralContructor e l alts
    where
      equalLit (LitPat l')     = l == l'
      equalLit _               = False


  -- The subject is an applied primitive
  (Prim _,_,_) ->
    -- We try to reduce the applied primitive to WHNF
    whnfRW True ctx subj $ \ctx1 subj1 -> case collectArgsTicks subj1 of
      -- WHNF of subject is a literal, try `caseCon` with that
      (Literal l,_,_) -> caseCon ctx1 (Case (Literal l) ty alts)
      -- WHNF of subject is a data-constructor, try `caseCon` with that
      (Data _,_,_) -> caseCon ctx1 (Case subj1 ty alts)
#if MIN_VERSION_ghc(8,2,2)
      -- WHNF of subject is _|_, in the form of `absentError`: that means that
      -- the entire case-expression is evaluates to _|_
      (Prim pInfo,_:msgOrCallStack:_,ticks)
        | primName pInfo == "Control.Exception.Base.absentError" ->
        let e1 = mkApps (mkTicks (Prim pInfo) ticks)
                        [Right ty,msgOrCallStack]
        in  changed e1
#endif
      -- WHNF of subject is _|_, in the form of `absentError`, `patError`,
      -- or `undefined`: that means the entire case-expression is _|_
      (Prim pInfo,repTy:_:msgOrCallStack:_,ticks)
        | primName pInfo `elem` ["Control.Exception.Base.patError"
#if !MIN_VERSION_ghc(8,2,2)
                                ,"Control.Exception.Base.absentError"
#endif
                                ,"GHC.Err.undefined"] ->
        let e1 = mkApps (mkTicks (Prim pInfo) ticks)
                        [repTy,Right ty,msgOrCallStack]
        in  changed e1
      -- WHNF of subject is _|_, in the form of our internal _|_-values: that
      -- means the entire case-expression is _|_
      (Prim pInfo,[_],ticks)
        | primName pInfo `elem` [ "Clash.Transformations.undefined"
                                , "Clash.GHC.Evaluator.undefined"
                                , "EmptyCase"] ->
        let e1 = mkApps (mkTicks (Prim pInfo) ticks) [Right ty]
        in changed e1
      -- WHNF of subject is non of the above, so either a variable reference,
      -- or a primitive for which the evaluator doesn't have any evaluation
      -- rules.
      _ -> do
        let subjTy = termType tcm subj
        tran <- Lens.view typeTranslator
        reprs <- Lens.view customReprs
        case (`evalState` HashMapS.empty) (coreTypeToHWType tran reprs tcm subjTy) of
          Right (FilteredHWType (Void (Just hty)) _areVoids)
            | hty `elem` [BitVector 0, Unsigned 0, Signed 0, Index 1]
            -- If we know that the type of the subject is zero-bits wide and
            -- one of the Clash number types. Then the only valid alternative is
            -- the one that can match on the literal "0", so try 'caseCon' with
            -- that.
            -> caseCon ctx1 (Case (Literal (IntegerLiteral 0)) ty alts)
          _ -> do
            let ret = caseOneAlt e
            -- Otherwise check whether the entire case-expression has a single
            -- alternative, and pick that one.
            lvl <- Lens.view dbgLevel
            if lvl > DebugNone then do
              let subjIsConst = isConstant subj
              -- In debug mode we always report missing evaluation rules for the
              -- primitive evaluator
              traceIf (lvl > DebugNone && subjIsConst)
                      ("Irreducible constant as case subject: " ++ showPpr subj ++
                       "\nCan be reduced to: " ++ showPpr subj1) ret
            else
              ret


  -- The subject is a variable
  (Var v, [], _) | isNum0 (varType v) ->
    -- If we know that the type of the subject is zero-bits wide and
    -- one of the Clash number types. Then the only valid alternative is
    -- the one that can match on the literal "0", so try 'caseCon' with
    -- that.
    caseCon ctx (Case (Literal (IntegerLiteral 0)) ty alts)
   where
    isNum0 (tyView -> TyConApp (nameOcc -> tcNm) [arg])
      | tcNm `elem`
        ["Clash.Sized.Internal.BitVector.BitVector"
        ,"Clash.Sized.Internal.Unsigned.Unsigned"
        ,"Clash.Sized.Internal.Signed.Signed"
        ]
      = isLitX 0 arg
      | tcNm ==
        "Clash.Sized.Internal.Index.Index"
      = isLitX 1 arg
    isNum0 (coreView1 tcm -> Just t) = isNum0 t
    isNum0 _ = False

    isLitX n (LitTy (NumTy m)) = n == m
    isLitX n (coreView1 tcm -> Just t) = isLitX n t
    isLitX _ _ = False

  -- Otherwise check whether the entire case-expression has a single
  -- alternative, and pick that one.
  _ -> caseOneAlt e

caseCon _ e = return e
{-# SCC caseCon #-}

{- [Note: Name re-creation]
The names of heap bound variables are safely generate with mkUniqSystemId in Clash.Core.Evaluator.newLetBinding.
But only their uniqs end up in the heap, not the complete names.
So we use mkUnsafeSystemName to recreate the same Name.
-}

matchLiteralContructor
  :: Term
  -> Literal
  -> [(Pat,Term)]
  -> NormalizeSession Term
matchLiteralContructor c (IntegerLiteral l) alts = go (reverse alts)
 where
  go [(DefaultPat,e)] = changed e
  go ((DataPat dc [] xs,e):alts')
    | dcTag dc == 1
    , l >= ((-2)^(63::Int)) &&  l < 2^(63::Int)
    = let fvs       = Lens.foldMapOf freeLocalIds unitVarSet e
          (binds,_) = List.partition ((`elemVarSet` fvs) . fst)
                    $ zip xs [Literal (IntLiteral l)]
          e' = case binds of
                 [] -> e
                 _  -> Letrec binds e
      in changed e'
    | dcTag dc == 2
    , l >= 2^(63::Int)
    = let !(Jp# !(BN# ba)) = l
          ba'       = BA.ByteArray ba
          fvs       = Lens.foldMapOf freeLocalIds unitVarSet e
          (binds,_) = List.partition ((`elemVarSet` fvs) . fst)
                    $ zip xs [Literal (ByteArrayLiteral ba')]
          e' = case binds of
                 [] -> e
                 _  -> Letrec binds e
      in changed e'
    | dcTag dc == 3
    , l < ((-2)^(63::Int))
    = let !(Jn# !(BN# ba)) = l
          ba'       = BA.ByteArray ba
          fvs       = Lens.foldMapOf freeLocalIds unitVarSet e
          (binds,_) = List.partition ((`elemVarSet` fvs) . fst)
                    $ zip xs [Literal (ByteArrayLiteral ba')]
          e' = case binds of
                 [] -> e
                 _  -> Letrec binds e
      in changed e'
    | otherwise
    = go alts'
  go ((LitPat l', e):alts')
    | IntegerLiteral l == l'
    = changed e
    | otherwise
    = go alts'
  go _ = error $ $(curLoc) ++ "Report as bug: caseCon error: " ++ showPpr c

matchLiteralContructor c (NaturalLiteral l) alts = go (reverse alts)
 where
  go [(DefaultPat,e)] = changed e
  go ((DataPat dc [] xs,e):alts')
    | dcTag dc == 1
    , l >= 0 && l < 2^(64::Int)
    = let fvs       = Lens.foldMapOf freeLocalIds unitVarSet e
          (binds,_) = List.partition ((`elemVarSet` fvs) . fst)
                    $ zip xs [Literal (WordLiteral l)]
          e' = case binds of
                 [] -> e
                 _  -> Letrec binds e
      in changed e'
    | dcTag dc == 2
    , l >= 2^(64::Int)
    = let !(Jp# !(BN# ba)) = l
          ba'       = BA.ByteArray ba
          fvs       = Lens.foldMapOf freeLocalIds unitVarSet e
          (binds,_) = List.partition ((`elemVarSet` fvs) . fst)
                    $ zip xs [Literal (ByteArrayLiteral ba')]
          e' = case binds of
                 [] -> e
                 _  -> Letrec binds e
      in changed e'
    | otherwise
    = go alts'
  go ((LitPat l', e):alts')
    | NaturalLiteral l == l'
    = changed e
    | otherwise
    = go alts'
  go _ = error $ $(curLoc) ++ "Report as bug: caseCon error: " ++ showPpr c

matchLiteralContructor _ _ ((DefaultPat,e):_) = changed e
matchLiteralContructor c _ _ =
  error $ $(curLoc) ++ "Report as bug: caseCon error: " ++ showPpr c
{-# SCC matchLiteralContructor #-}

caseOneAlt :: Term -> RewriteMonad extra Term
caseOneAlt e@(Case _ _ [(pat,altE)]) = case pat of
  DefaultPat -> changed altE
  LitPat _ -> changed altE
  DataPat _ tvs xs
    | (coerce tvs ++ coerce xs) `localVarsDoNotOccurIn` altE
    -> changed altE
    | otherwise
    -> return e

caseOneAlt (Case _ _ alts@((_,alt):_:_))
  | all ((== alt) . snd) (tail alts)
  = changed alt

caseOneAlt e = return e
{-# SCC caseOneAlt #-}

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

-- | Ensure that top-level lambda's eventually bind a let-expression of which
-- the body is a variable-reference.
topLet :: HasCallStack => NormRewrite
topLet (TransformContext is0 ctx) e
  | all (\c -> isLambdaBodyCtx c || isTickCtx c) ctx && not (isLet e) && not (isTick e)
  = do
  untranslatable <- isUntranslatable False e
  if untranslatable
    then return e
    else do tcm <- Lens.view tcCache
            argId <- mkTmBinderFor is0 tcm (mkUnsafeSystemName "result" 0) e
            changed (Letrec [(argId, e)] (Var argId))
 where
  isTick Tick{} = True
  isTick _ = False

topLet (TransformContext is0 ctx) e@(Letrec binds body)
  | all (\c -> isLambdaBodyCtx c || isTickCtx c) ctx
  = do
    let localVar = isLocalVar body
    untranslatable <- isUntranslatable False body
    if localVar || untranslatable
      then return e
      else do
        tcm <- Lens.view tcCache
        let is2 = extendInScopeSetList is0 (map fst binds)
        argId <- mkTmBinderFor is2 tcm (mkUnsafeSystemName "result" 0) body
        changed (Letrec (binds ++ [(argId,body)]) (Var argId))

topLet _ e = return e
{-# SCC topLet #-}

-- Misc rewrites

-- | Remove unused let-bindings
deadCode :: HasCallStack => NormRewrite
deadCode _ e@(Letrec binds body) =
  case removeUnusedBinders binds body of
    Just t -> changed t
    Nothing -> return e
deadCode _ e = return e
{-# SCC deadCode #-}

removeUnusedExpr :: HasCallStack => NormRewrite
removeUnusedExpr _ e@(collectArgsTicks -> (p@(Prim pInfo),args,ticks)) = do
  bbM <- HashMap.lookup (primName pInfo) <$> Lens.use (extra.primitives)
  let
    usedArgs0 =
      case Monad.join (extractPrim <$> bbM) of
        Just (BlackBoxHaskell{usedArguments}) ->
          case usedArguments of
            UsedArguments used -> Just used
            IgnoredArguments ignored -> Just ([0..length args - 1] \\ ignored)
        Just (BlackBox pNm _ _ _ _ _ _ _ _ inc r ri templ) -> Just $
          if | isFromInt pNm -> [0,1,2]
             | primName pInfo `elem` [ "Clash.Annotations.BitRepresentation.Deriving.dontApplyInHDL"
                                     , "Clash.Sized.Vector.splitAt"
                                     ] -> [0,1]
             | otherwise -> concat [ maybe [] getUsedArguments r
                                   , maybe [] getUsedArguments ri
                                   , getUsedArguments templ
                                   , concatMap (getUsedArguments . snd) inc ]
        _ ->
          Nothing

  case usedArgs0 of
    Nothing ->
      return e
    Just usedArgs1 -> do
      tcm <- Lens.view tcCache
      (args1, Monoid.getAny -> hasChanged) <- listen (go tcm 0 usedArgs1 args)
      if hasChanged then
        return (mkApps (mkTicks p ticks) args1)
      else
        return e

  where
    arity = length . Either.rights . fst $ splitFunForallTy (primType pInfo)

    go _ _ _ [] = return []
    go tcm !n used (Right ty:args') = do
      args'' <- go tcm n used args'
      return (Right ty : args'')
    go tcm !n used (Left tm : args') = do
      args'' <- go tcm (n+1) used args'
      case tm of
        TyApp (Prim p0) _
          | primName p0 == "Clash.Transformations.removedArg"
          -> return (Left tm : args'')
        _ -> do
          let ty = termType tcm tm
              p' = removedTm ty
          if n < arity && n `notElem` used
             then changed (Left p' : args'')
             else return  (Left tm : args'')

removeUnusedExpr _ e@(Case _ _ [(DataPat _ [] xs,altExpr)]) =
  if xs `localIdsDoNotOccurIn` altExpr
     then changed altExpr
     else return e

-- Replace any expression that creates a Vector of size 0 within the application
-- of the Cons constructor, by the Nil constructor.
removeUnusedExpr _ e@(collectArgsTicks -> (Data dc, [_,Right aTy,Right nTy,_,Left a,Left nil],ticks))
  | nameOcc (dcName dc) == "Clash.Sized.Vector.Cons"
  = do
    tcm <- Lens.view tcCache
    case runExcept (tyNatSize tcm nTy) of
      Right 0
        | (con, _) <- collectArgs nil
        , not (isCon con)
        -> let eTy = termType tcm e
               (TyConApp vecTcNm _) = tyView eTy
               (Just vecTc) = lookupUniqMap vecTcNm tcm
               [nilCon,consCon] = tyConDataCons vecTc
               v = mkTicks (mkVec nilCon consCon aTy 1 [a]) ticks
           in  changed v
      _ -> return e

removeUnusedExpr _ e = return e
{-# SCC removeUnusedExpr #-}

-- | Inline let-bindings when the RHS is either a local variable reference or
-- is constant (except clock or reset generators)
bindConstantVar :: HasCallStack => NormRewrite
bindConstantVar = inlineBinders test
  where
    test _ (i,stripTicks -> e) = case isLocalVar e of
      -- Don't inline `let x = x in x`, it throws  us in an infinite loop
      True -> return (i `localIdDoesNotOccurIn` e)
      _    -> isWorkFreeIsh e >>= \case
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
argCastSpec ctx e@(App _ (stripTicks -> Cast e' _ _)) =
  isWorkFree e' >>= \case
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
    let isSignal = isSignalType tcm eTy
    let lv = isLocalId f
    if untranslatable || isSignal || argsHaveWork || lv
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
    orM [pure (isVar arg), isWorkFree arg] >>= \case
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
    let argTy = termType tcm arg
        ty1   = applyFunTy tcm ty0 argTy
    orM [pure (isVar arg), isWorkFree arg] >>= \case
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

-- | Flatten ridiculous case-statements generated by GHC
--
-- For case-statements in haskell of the form:
--
-- @
-- f :: Unsigned 4 -> Unsigned 4
-- f x = case x of
--   0 -> 3
--   1 -> 2
--   2 -> 1
--   3 -> 0
-- @
--
-- GHC generates Core that looks like:
--
-- @
-- f = \(x :: Unsigned 4) -> case x == fromInteger 3 of
--                             False -> case x == fromInteger 2 of
--                               False -> case x == fromInteger 1 of
--                                 False -> case x == fromInteger 0 of
--                                   False -> error "incomplete case"
--                                   True  -> fromInteger 3
--                                 True -> fromInteger 2
--                               True -> fromInteger 1
--                             True -> fromInteger 0
-- @
--
-- Which would result in a priority decoder circuit where a normal decoder
-- circuit was desired.
--
-- This transformation transforms the above Core to the saner:
--
-- @
-- f = \(x :: Unsigned 4) -> case x of
--        _ -> error "incomplete case"
--        0 -> fromInteger 3
--        1 -> fromInteger 2
--        2 -> fromInteger 1
--        3 -> fromInteger 0
-- @
caseFlat :: HasCallStack => NormRewrite
caseFlat _ e@(Case (collectEqArgs -> Just (scrut',_)) ty _)
  = do
       case collectFlat scrut' e of
         Just alts' -> changed (Case scrut' ty (last alts' : init alts'))
         Nothing    -> return e

caseFlat _ e = return e
{-# SCC caseFlat #-}

collectFlat :: Term -> Term -> Maybe [(Pat,Term)]
collectFlat scrut (Case (collectEqArgs -> Just (scrut', val)) _ty [lAlt,rAlt])
  | scrut' == scrut
  = case collectArgs val of
      (Prim p,args') | isFromInt (primName p) ->
        go (last args')
      (Data dc,args')    | nameOcc (dcName dc) == "GHC.Types.I#" ->
        go (last args')
      _ -> Nothing
  where
    go (Left (Literal i)) = case (lAlt,rAlt) of
              ((pl,el),(pr,er))
                | isFalseDcPat pl || isTrueDcPat pr ->
                   case collectFlat scrut el of
                     Just alts' -> Just ((LitPat i, er) : alts')
                     Nothing    -> Just [(LitPat i, er)
                                        ,(DefaultPat, el)
                                        ]
                | otherwise ->
                   case collectFlat scrut er of
                     Just alts' -> Just ((LitPat i, el) : alts')
                     Nothing    -> Just [(LitPat i, el)
                                        ,(DefaultPat, er)
                                        ]
    go _ = Nothing

    isFalseDcPat (DataPat p _ _)
      = ((== "GHC.Types.False") . nameOcc . dcName) p
    isFalseDcPat _ = False

    isTrueDcPat (DataPat p _ _)
      = ((== "GHC.Types.True") . nameOcc . dcName) p
    isTrueDcPat _ = False

collectFlat _ _ = Nothing
{-# SCC collectFlat #-}

collectEqArgs :: Term -> Maybe (Term,Term)
collectEqArgs (collectArgsTicks -> (Prim p, args, ticks))
  | nm == "Clash.Sized.Internal.BitVector.eq#"
    = let [_,_,Left scrut,Left val] = args
      in Just (mkTicks scrut ticks,val)
  | nm == "Clash.Sized.Internal.Index.eq#"  ||
    nm == "Clash.Sized.Internal.Signed.eq#" ||
    nm == "Clash.Sized.Internal.Unsigned.eq#"
    = let [_,Left scrut,Left val] = args
      in Just (mkTicks scrut ticks,val)
  | nm == "Clash.Transformations.eqInt"
    = let [Left scrut,Left val] = args
      in  Just (mkTicks scrut ticks,val)
 where
  nm = primName p

collectEqArgs _ = Nothing

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
    untranslatable <- lift (isUntranslatable False arg)
    let localVar   = isLocalVar arg
    constantNoCR   <- lift (isConstantNotClockReset arg)
    -- See Note [ANF no let-bind]
    case (untranslatable,localVar || constantNoCR, isSimBind conVarPrim,arg) of
      (False,False,False,_) -> do
        tcm <- Lens.view tcCache
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

isClassConstraint :: Type -> Bool
isClassConstraint (tyView -> TyConApp nm0 _) =
  if -- Constraint tuple:
     | "GHC.Classes.(%" `Text.isInfixOf` nm1 -> True
     -- Constraint class:
     | "C:" `Text.isInfixOf` nm2 -> True
     | otherwise -> False
 where
  nm1 = nameOcc nm0
  nm2 = snd (Text.breakOnEnd "." nm1)

isClassConstraint _ = False


-- | Turn a  normalized recursive function, where the recursive calls only pass
-- along the unchanged original arguments, into let-recursive function. This
-- means that all recursive calls are replaced by the same variable reference as
-- found in the body of the top-level let-expression.
recToLetRec :: HasCallStack => NormRewrite
recToLetRec (TransformContext is0 []) e = do
  (fn,_) <- Lens.use curFun
  tcm    <- Lens.view tcCache
  case splitNormalized tcm e of
    Right (args,bndrs,res) -> do
      let args'             = map Var args
          (toInline,others) = List.partition (eqApp tcm fn args' . snd) bndrs
          resV              = Var res
      case (toInline,others) of
        (_:_,_:_) -> do
          let is1          = extendInScopeSetList is0 (args ++ map fst bndrs)
          let substsInline = extendIdSubstList (mkSubst is1)
                           $ map (second (const resV)) toInline
              others'      = map (second (substTm "recToLetRec" substsInline))
                                 others
          changed $ mkLams (Letrec others' resV) args
        _ -> return e
    _ -> return e
  where
    -- This checks whether things are semantically equal. For example, say we
    -- have:
    --
    --   x :: (a, (b, c))
    --
    -- and
    --
    --   y :: (a, (b, c))
    --
    -- If we can determine that 'y' is constructed solely using the
    -- corresponding fields in 'x', then we can say they are semantically
    -- equal. The algorithm below keeps track of what (sub)field it is
    -- constructing, and checks if the field-expression projects the
    -- corresponding (sub)field from the target variable.
    --
    -- TODO: See [Note: Breaks on constants and predetermined equality]
    eqApp tcm v args (collectArgs . stripTicks -> (Var v',args'))
      | isGlobalId v'
      , v == v'
      , let args2 = Either.lefts args'
      , length args == length args2
      = and (zipWith (eqArg tcm) args args2)
    eqApp _ _ _ _ = False

    eqArg _ v1 v2@(stripTicks -> Var {})
      = v1 == v2
    eqArg tcm v1 v2@(collectArgs . stripTicks -> (Data _, args'))
      | let t1 = normalizeType tcm (termType tcm v1)
      , let t2 = normalizeType tcm (termType tcm v2)
      , t1 == t2
      = if isClassConstraint t1 then
          -- Class constraints are equal if their types are equal, so we can
          -- take a shortcut here.
          True
        else
          -- Check whether all arguments to the data constructor are projections
          --
          and (zipWith (eqDat v1) (map pure [0..]) (Either.lefts args'))
    eqArg _ _ _
      = False

    -- Recursively check whether a term /e/ is semantically equal to some variable /v/.
    -- Currently it can only assert equality when /e/ is  syntactically equal
    -- to /v/, or is constructed out of projections of /v/, importantly:
    --
    -- [Note: Breaks on constants and predetermined equality]
    -- This function currently breaks if:
    --
    --   * One or more subfields are constants. Constants might have been
    --     inlined for the construction, instead of being a projection of the
    --     target variable.
    --
    --   * One or more subfields are determined to be equal and one is simply
    --     swapped / replaced by the other. For example, say we have
    --     `x :: (a, a)`. If GHC determines that both elements of the tuple will
    --     always be the same, it might replace the (semantically equal to 'x')
    --     construction of `y` with `(fst x, fst x)`.
    --
    eqDat :: Term -> [Int] -> Term -> Bool
    eqDat v fTrace (collectArgs . stripTicks -> (Data _, args)) =
      and (zipWith (eqDat v) (map (:fTrace) [0..]) (Either.lefts args))
    eqDat v1 fTrace v2 =
      case stripProjection (reverse fTrace) v1 v2 of
        Just [] -> True
        _ -> False

    stripProjection :: [Int] -> Term -> Term -> Maybe [Int]
    stripProjection fTrace0 vTarget0 (Case v _ [(DataPat _ _ xs, r)]) = do
      -- Get projection made in subject of case:
      fTrace1 <- stripProjection fTrace0 vTarget0 v

      -- Extract projection of this case statement. Subsequent calls to
      -- 'stripProjection' will check if new target is actually used.
      (n, fTrace2) <- List.uncons fTrace1
      vTarget1 <- List.indexMaybe xs n

      stripProjection fTrace2 (Var vTarget1) r

    stripProjection fTrace (Var sTarget) (Var s) =
      if sTarget == s then Just fTrace else Nothing

    stripProjection _fTrace _vTarget _v =
      Nothing

recToLetRec _ e = return e
{-# SCC recToLetRec #-}

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

-- | Simplified CSE, only works on let-bindings, does an inverse topological
-- sort of the let-bindings and then works from top to bottom
--
-- XXX: Check whether inverse top-sort followed by single traversal removes as
-- many binders as the previous "apply-until-fixpoint" approach in the presence
-- of recursive groups in the let-bindings. If not but just for checking whether
-- changes to transformation affect the eventual size of the circuit, it would
-- be really helpful if we tracked circuit size in the regression/test suite.
-- On the two examples that were tested, Reducer and PipelinesViaFolds, this new
-- version of CSE removed the same amount of let-binders.
simpleCSE :: HasCallStack => NormRewrite
simpleCSE (TransformContext is0 _) (inverseTopSortLetBindings -> Letrec bndrs body) = do
  let is1 = extendInScopeSetList is0 (map fst bndrs)
  (subst,bndrs1) <- reduceBinders (mkSubst is1) [] bndrs
  -- TODO: check whether a substitution over the body is enough, the reason I'm
  -- doing a substitution over the the binders as well is that I don't know in
  -- what order a recursive group shows up in a inverse topological sort.
  -- Depending on the order and forgetting to apply the substitution over the
  -- let-bindings might lead to the introduction of free variables.
  --
  -- NB: don't apply the substitution to the entire let-expression, and that
  -- would rename the let-bindings because they've been added to the InScopeSet
  -- of the substitution.
  let bndrs2 = map (second (substTm "simpleCSE.bndrs" subst)) bndrs1
      body1  = substTm "simpleCSE.body" subst body
  return (Letrec bndrs2 body1)

simpleCSE _ e = return e
{-# SCC simpleCSE #-}

-- | XXX: is given inverse topologically sorted binders, but returns
-- topologically sorted binders
--
-- TODO: check further speed improvements:
--
-- 1. Store the processed binders in a `Map Expr LetBinding`:
--    * Trades O(1) `cons` and O(n)*aeqTerm `find` for:
--    * O(log n)*aeqTerm `insert` and O(log n)*aeqTerm `lookup`
-- 2. Store the processed binders in a `AEQTrie Expr LetBinding`
--    * Trades O(1) `cons` and O(n)*aeqTerm `find` for:
--    * O(e) `insert` and O(e) `lookup`
reduceBinders
  :: Subst
  -> [LetBinding]
  -> [LetBinding]
  -> RewriteMonad NormalizeState (Subst, [LetBinding])
reduceBinders !subst processed [] = return (subst,processed)
reduceBinders !subst processed ((i,substTm "reduceBinders" subst -> e):rest)
  | (_,_,ticks) <- collectArgsTicks e
  , NoDeDup `notElem` ticks
  , Just (i1,_) <- List.find ((== e) . snd) processed
  = do
    let subst1 = extendIdSubst subst i (Var i1)
    setChanged
    reduceBinders subst1 processed rest
  | otherwise
  = reduceBinders subst ((i,e):processed) rest
{-# SCC reduceBinders #-}

reduceConst :: HasCallStack => NormRewrite
reduceConst ctx e@(App _ _)
  | (Prim p0, _) <- collectArgs e
  = whnfRW False ctx e $ \_ctx1 e1 -> case e1 of
      (collectArgs -> (Prim p1, _)) | primName p0 == primName p1 -> return e
      _ -> changed e1

reduceConst _ e = return e
{-# SCC reduceConst #-}

-- | Replace primitives by their "definition" if they would lead to let-bindings
-- with a non-representable type when a function is in ANF. This happens for
-- example when Clash.Size.Vector.map consumes or produces a vector of
-- non-representable elements.
--
-- Basically what this transformation does is replace a primitive the completely
-- unrolled recursive definition that it represents. e.g.
--
-- > zipWith ($) (xs :: Vec 2 (Int -> Int)) (ys :: Vec 2 Int)
--
-- is replaced by:
--
-- > let (x0  :: (Int -> Int))       = case xs  of (:>) _ x xr -> x
-- >     (xr0 :: Vec 1 (Int -> Int)) = case xs  of (:>) _ x xr -> xr
-- >     (x1  :: (Int -> Int)(       = case xr0 of (:>) _ x xr -> x
-- >     (y0  :: Int)                = case ys  of (:>) _ y yr -> y
-- >     (yr0 :: Vec 1 Int)          = case ys  of (:>) _ y yr -> xr
-- >     (y1  :: Int                 = case yr0 of (:>) _ y yr -> y
-- > in  (($) x0 y0 :> ($) x1 y1 :> Nil)
--
-- Currently, it only handles the following functions:
--
-- * Clash.Sized.Vector.zipWith
-- * Clash.Sized.Vector.map
-- * Clash.Sized.Vector.traverse#
-- * Clash.Sized.Vector.fold
-- * Clash.Sized.Vector.foldr
-- * Clash.Sized.Vector.dfold
-- * Clash.Sized.Vector.(++)
-- * Clash.Sized.Vector.head
-- * Clash.Sized.Vector.tail
-- * Clash.Sized.Vector.last
-- * Clash.Sized.Vector.init
-- * Clash.Sized.Vector.unconcat
-- * Clash.Sized.Vector.transpose
-- * Clash.Sized.Vector.replicate
-- * Clash.Sized.Vector.replace_int
-- * Clash.Sized.Vector.imap
-- * Clash.Sized.Vector.dtfold
-- * Clash.Sized.RTree.tdfold
-- * Clash.Sized.RTree.treplicate
-- * Clash.Sized.Internal.BitVector.split#
-- * Clash.Sized.Internal.BitVector.eq#
reduceNonRepPrim :: HasCallStack => NormRewrite
reduceNonRepPrim c@(TransformContext is0 ctx) e@(App _ _) | (Prim p, args, ticks) <- collectArgsTicks e = do
  tcm <- Lens.view tcCache
  ultra <- Lens.use (extra.normalizeUltra)
  let eTy = termType tcm e
  case tyView eTy of
    (TyConApp vecTcNm@(nameOcc -> "Clash.Sized.Vector.Vec")
              [runExcept . tyNatSize tcm -> Right 0, aTy]) -> do
      let (Just vecTc) = lookupUniqMap vecTcNm tcm
          [nilCon,consCon] = tyConDataCons vecTc
          nilE = mkVec nilCon consCon aTy 0 []
      changed (mkTicks nilE ticks)
    tv -> let argLen = length args in case primName p of
      "Clash.Sized.Vector.zipWith" | argLen == 7 -> do
        let [lhsElTy,rhsElty,resElTy,nTy] = Either.rights args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ pure (ultra || n < 2)
                                 , shouldReduce ctx
                                 , List.anyM isUntranslatableType_not_poly
                                        [lhsElTy,rhsElty,resElTy] ]
            if shouldReduce1
               then let [fun,lhsArg,rhsArg] = Either.lefts args
                    in  (`mkTicks` ticks) <$>
                        reduceZipWith c p n lhsElTy rhsElty resElTy fun lhsArg rhsArg
               else return e
          _ -> return e
      "Clash.Sized.Vector.map" | argLen == 5 -> do
        let [argElTy,resElTy,nTy] = Either.rights args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ pure (ultra || n < 2 )
                                 , shouldReduce ctx
                                 , List.anyM isUntranslatableType_not_poly
                                        [argElTy,resElTy] ]
            if shouldReduce1
               then let [fun,arg] = Either.lefts args
                    in  (`mkTicks` ticks) <$> reduceMap c p n argElTy resElTy fun arg
               else return e
          _ -> return e
      "Clash.Sized.Vector.traverse#" | argLen == 7 ->
        let [aTy,fTy,bTy,nTy] = Either.rights args
        in  case runExcept (tyNatSize tcm nTy) of
          Right n ->
            let [dict,fun,arg] = Either.lefts args
            in  (`mkTicks` ticks) <$> reduceTraverse c n aTy fTy bTy dict fun arg
          _ -> return e
      "Clash.Sized.Vector.fold" | argLen == 4 -> do
        let [nTy,aTy] = Either.rights args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ pure (ultra || n == 0)
                                 , shouldReduce ctx
                                 , isUntranslatableType_not_poly aTy ]
            if shouldReduce1 then
              let [fun,arg] = Either.lefts args
              in  (`mkTicks` ticks) <$> reduceFold c (n + 1) aTy fun arg
            else return e
          _ -> return e
      "Clash.Sized.Vector.foldr" | argLen == 6 ->
        let [aTy,bTy,nTy] = Either.rights args
        in  case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ pure ultra
                                 , shouldReduce ctx
                                 , List.anyM isUntranslatableType_not_poly [aTy,bTy] ]
            if shouldReduce1
              then let [fun,start,arg] = Either.lefts args
                   in  (`mkTicks` ticks) <$> reduceFoldr c p n aTy fun start arg
              else return e
          _ -> return e
      "Clash.Sized.Vector.dfold" | argLen == 8 ->
        let ([_kn,_motive,fun,start,arg],[_mTy,nTy,aTy]) = Either.partitionEithers args
        in  case runExcept (tyNatSize tcm nTy) of
          Right n -> (`mkTicks` ticks) <$> reduceDFold is0 n aTy fun start arg
          _ -> return e
      "Clash.Sized.Vector.++" | argLen == 5 ->
        let [nTy,aTy,mTy] = Either.rights args
            [lArg,rArg]   = Either.lefts args
        in case (runExcept (tyNatSize tcm nTy), runExcept (tyNatSize tcm mTy)) of
              (Right n, Right m)
                | n == 0 -> changed rArg
                | m == 0 -> changed lArg
                | otherwise -> do
                    shouldReduce1 <- List.orM [ shouldReduce ctx
                                         , isUntranslatableType_not_poly aTy ]
                    if shouldReduce1
                       then (`mkTicks` ticks) <$> reduceAppend is0 n m aTy lArg rArg
                       else return e
              _ -> return e
      "Clash.Sized.Vector.head" | argLen == 3 -> do
        let [nTy,aTy] = Either.rights args
            [vArg]    = Either.lefts args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ shouldReduce ctx
                                 , isUntranslatableType_not_poly aTy ]
            if shouldReduce1
               then (`mkTicks` ticks) <$> reduceHead is0 (n+1) aTy vArg
               else return e
          _ -> return e
      "Clash.Sized.Vector.tail" | argLen == 3 -> do
        let [nTy,aTy] = Either.rights args
            [vArg]    = Either.lefts args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ shouldReduce ctx
                                 , isUntranslatableType_not_poly aTy ]
            if shouldReduce1
               then (`mkTicks` ticks) <$> reduceTail is0 (n+1) aTy vArg
               else return e
          _ -> return e
      "Clash.Sized.Vector.last" | argLen == 3 -> do
        let [nTy,aTy] = Either.rights args
            [vArg]    = Either.lefts args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ shouldReduce ctx
                                 , isUntranslatableType_not_poly aTy
                                 ]
            if shouldReduce1
               then (`mkTicks` ticks) <$> reduceLast is0 (n+1) aTy vArg
               else return e
          _ -> return e
      "Clash.Sized.Vector.init" | argLen == 3 -> do
        let [nTy,aTy] = Either.rights args
            [vArg]    = Either.lefts args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ shouldReduce ctx
                                 , isUntranslatableType_not_poly aTy ]
            if shouldReduce1
               then (`mkTicks` ticks) <$> reduceInit is0 p n aTy vArg
               else return e
          _ -> return e
      "Clash.Sized.Vector.unconcat" | argLen == 6 -> do
        let ([_knN,_sm,arg],[mTy,nTy,aTy]) = Either.partitionEithers args
        case (runExcept (tyNatSize tcm nTy), runExcept (tyNatSize tcm mTy)) of
          (Right n, Right 0) -> (`mkTicks` ticks) <$> reduceUnconcat n 0 aTy arg
          _ -> return e
      "Clash.Sized.Vector.transpose" | argLen == 5 -> do
        let ([_knN,arg],[mTy,nTy,aTy]) = Either.partitionEithers args
        case (runExcept (tyNatSize tcm nTy), runExcept (tyNatSize tcm mTy)) of
          (Right n, Right 0) -> (`mkTicks` ticks) <$> reduceTranspose n 0 aTy arg
          _ -> return e
      "Clash.Sized.Vector.replicate" | argLen == 4 -> do
        let ([_sArg,vArg],[nTy,aTy]) = Either.partitionEithers args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ shouldReduce ctx
                                 , isUntranslatableType_not_poly aTy
                                 ]
            if shouldReduce1
               then (`mkTicks` ticks) <$> reduceReplicate n aTy eTy vArg
               else return e
          _ -> return e
       -- replace_int :: KnownNat n => Vec n a -> Int -> a -> Vec n a
      "Clash.Sized.Vector.replace_int" | argLen == 6 -> do
        let ([_knArg,vArg,iArg,aArg],[nTy,aTy]) = Either.partitionEithers args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ pure ultra
                                 , shouldReduce ctx
                                 , isUntranslatableType_not_poly aTy
                                 ]
            if shouldReduce1
               then (`mkTicks` ticks) <$> reduceReplace_int is0 n aTy eTy vArg iArg aArg
               else return e
          _ -> return e

      "Clash.Sized.Vector.index_int" | argLen == 5 -> do
        let ([_knArg,vArg,iArg],[nTy,aTy]) = Either.partitionEithers args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ pure ultra
                                 , shouldReduce ctx
                                 , isUntranslatableType_not_poly aTy ]
            if shouldReduce1
               then (`mkTicks` ticks) <$> reduceIndex_int is0 n aTy vArg iArg
               else return e
          _ -> return e

      "Clash.Sized.Vector.imap" | argLen == 6 -> do
        let [nTy,argElTy,resElTy] = Either.rights args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ pure (ultra || n < 2)
                                 , shouldReduce ctx
                                 , List.anyM isUntranslatableType_not_poly [argElTy,resElTy] ]
            if shouldReduce1
               then let [_,fun,arg] = Either.lefts args
                    in  (`mkTicks` ticks) <$> reduceImap c n argElTy resElTy fun arg
               else return e
          _ -> return e
      "Clash.Sized.Vector.iterateI" | argLen == 5 ->
        let ([_kn,f,a],[nTy,aTy]) = Either.partitionEithers args in
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM
              [ pure (ultra || n < 2)
              , shouldReduce ctx
              , isUntranslatableType_not_poly aTy ]

            if shouldReduce1 then
              (`mkTicks` ticks) <$> reduceIterateI c n aTy eTy f a
            else
              return e
          _ -> return e
      "Clash.Sized.Vector.dtfold" | argLen == 8 ->
        let ([_kn,_motive,lrFun,brFun,arg],[_mTy,nTy,aTy]) = Either.partitionEithers args
        in  case runExcept (tyNatSize tcm nTy) of
          Right n -> (`mkTicks` ticks) <$> reduceDTFold is0 n aTy lrFun brFun arg
          _ -> return e

      "Clash.Sized.Vector.reverse"
        | ultra
        , ([vArg],[nTy,aTy]) <- Either.partitionEithers args
        , Right n <- runExcept (tyNatSize tcm nTy)
        -> (`mkTicks` ticks) <$> reduceReverse is0 n aTy vArg

      "Clash.Sized.RTree.tdfold" | argLen == 8 ->
        let ([_kn,_motive,lrFun,brFun,arg],[_mTy,nTy,aTy]) = Either.partitionEithers args
        in  case runExcept (tyNatSize tcm nTy) of
          Right n -> (`mkTicks` ticks) <$> reduceTFold is0 n aTy lrFun brFun arg
          _ -> return e
      "Clash.Sized.RTree.treplicate" | argLen == 4 -> do
        let ([_sArg,vArg],[nTy,aTy]) = Either.partitionEithers args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ shouldReduce ctx
                                 , isUntranslatableType False aTy ]
            if shouldReduce1
               then (`mkTicks` ticks) <$> reduceTReplicate n aTy eTy vArg
               else return e
          _ -> return e
      "Clash.Sized.Internal.BitVector.split#" | argLen == 4 -> do
        let ([_knArg,bvArg],[nTy,mTy]) = Either.partitionEithers args
        case (runExcept (tyNatSize tcm nTy), runExcept (tyNatSize tcm mTy), tv) of
          (Right n, Right m, TyConApp tupTcNm [lTy,rTy])
            | n == 0 -> do
              let (Just tupTc) = lookupUniqMap tupTcNm tcm
                  [tupDc]      = tyConDataCons tupTc
                  tup          = mkApps (Data tupDc)
                                    [Right lTy
                                    ,Right rTy
                                    ,Left  bvArg
                                    ,Left  (removedTm rTy)
                                    ]

              changed (mkTicks tup ticks)
            | m == 0 -> do
              let (Just tupTc) = lookupUniqMap tupTcNm tcm
                  [tupDc]      = tyConDataCons tupTc
                  tup          = mkApps (Data tupDc)
                                    [Right lTy
                                    ,Right rTy
                                    ,Left  (removedTm lTy)
                                    ,Left  bvArg
                                    ]

              changed (mkTicks tup ticks)
          _ -> return e
      "Clash.Sized.Internal.BitVector.eq#"
        | ([_,_],[nTy]) <- Either.partitionEithers args
        , Right 0 <- runExcept (tyNatSize tcm nTy)
        , TyConApp boolTcNm [] <- tv
        -> let (Just boolTc) = lookupUniqMap boolTcNm tcm
               [_falseDc,trueDc] = tyConDataCons boolTc
           in  changed (mkTicks (Data trueDc) ticks)
      _ -> return e
  where
    isUntranslatableType_not_poly t = do
      u <- isUntranslatableType False t
      if u
         then return (null $ Lens.toListOf typeFreeVars t)
         else return False

reduceNonRepPrim _ e = return e
{-# SCC reduceNonRepPrim #-}

-- | This transformation lifts applications of global binders out of
-- alternatives of case-statements.
--
-- e.g. It converts:
--
-- @
-- case x of
--   A -> f 3 y
--   B -> f x x
--   C -> h x
-- @
--
-- into:
--
-- @
-- let f_arg0 = case x of {A -> 3; B -> x}
--     f_arg1 = case x of {A -> y; B -> x}
--     f_out  = f f_arg0 f_arg1
-- in  case x of
--       A -> f_out
--       B -> f_out
--       C -> h x
-- @
disjointExpressionConsolidation :: HasCallStack => NormRewrite
disjointExpressionConsolidation ctx@(TransformContext isCtx _) e@(Case _scrut _ty _alts@(_:_:_)) = do
    -- Collect all (the applications of) global binders (and certain primitives)
    -- that would be interesting to share out of the case-alternatives.
    (_,isCollected,collected) <- collectGlobals isCtx [] [] e
    -- Filter those that are used at most once in every (nested) branch.
    let disJoint = filter (isDisjoint . snd . snd) collected
    if null disJoint
       then return e
       else do
         -- For every to-lift expression create (the generalization of):
         --
         -- let fargs = case x of {A -> (3,y); B -> (x,x)}
         -- in  f (fst fargs) (snd fargs)
         --
         -- the let-expression is not created when `f` has only one (selectable)
         -- argument
         --
         -- NB: mkDisJointGroup needs the context InScopeSet, isCtx, to determine
         -- whether expressions reference variables from the context, or
         -- variables inside a let-expression inside one of the alternatives.
         lifted <- mapM (mkDisjointGroup isCtx) disJoint
         tcm    <- Lens.view tcCache
         -- Create let-binders for all of the lifted expressions
         --
         -- NB: Because we will be substituting under binders we use the collected
         -- inScopeSet, isCollected, which also contains all the binders
         -- created inside all of the alternatives. With this inScopeSet, we
         -- ensure that the let-bindings we create here won't be accidentally
         -- captured by binders inside the case-alternatives.
         (_,funOutIds) <- List.mapAccumLM (mkFunOut tcm)
                                          isCollected
                                          (zip disJoint lifted)
         -- Create "substitutions" of the form [f X Y := f_out]
         let substitution = zip (map fst disJoint) (map Var funOutIds)
         -- For all of the lifted expression: substitute occurrences of the
         -- disjoint expressions (f X Y) by a variable reference to the lifted
         -- expression (f_out)
         let isCtx1 = extendInScopeSetList isCtx funOutIds
         lifted1 <- substLifted isCtx1 substitution lifted
         -- Do the same for the actual case expression
         (e1,_,_) <- collectGlobals isCtx1 substitution [] e
         -- Let-bind all the lifted function
         let lb = Letrec (zip funOutIds lifted1) e1
         -- Do an initial dead-code elimination pass, as `mkDisJoint` doesn't
         -- clean-up unused let-binders.
         lb1 <- bottomupR deadCode ctx lb
         changed lb1
  where
    -- Make the let-binder for the lifted expressions
    mkFunOut tcm isN ((fun,_),(eLifted,_)) = do
      let ty  = termType tcm eLifted
          nm  = case collectArgs fun of
                   (Var v,_)  -> nameOcc (varName v)
                   (Prim p,_) -> primName p
                   _          -> "complex_expression_"
          nm1 = last (Text.splitOn "." nm) `Text.append` "Out"
      nm2 <- mkInternalVar isN nm1 ty
      return (extendInScopeSet isN nm2,nm2)

    -- Substitute inside the lifted expressions
    --
    -- In case you are wondering why this function isn't simply
    --
    -- > mapM (\s (eL,seen) -> collectGlobal isN s seen eL) substitution lifted
    --
    -- then that's because we have e.g. the list of "substitutions":
    --
    -- [foo _ _ := foo_out; bar _ _ := bar_out]
    --
    -- and if we were to apply that to a lifted expression, which is going
    -- to be of the form `foo (case ...) (case ...)` then we would end up
    -- with let-bindings that are simply:
    --
    -- > let foo_out = foo_out ; bar_out = bar_out
    --
    -- instead of the desired
    --
    -- > let foo_out = foo ((case ...)[foo _ _ := foo_out; bar _ _ := bar_out])
    -- >                   ((case ...)[foo _ _ := foo_out; bar _ _ := bar_out])
    -- >     bar_out = bar ((case ...)[foo _ _ := foo_out; bar _ _ := bar_out])
    -- >                   ((case ...)[foo _ _ := foo_out; bar _ _ := bar_out])
    --
    -- So what we do is that for every lifted-expression we make sure that the
    -- 'substitution' never contains the self-substitution, so we end up with:
    --
    -- > let foo_out = (foo (case ...) (case ...))[bar _ _ := bar_out]
    --       bar_out = (bar (case ...) (case ...))[foo _ _ := foo_out]
    --
    -- We used to have a different approach, see commit
    -- 73d237017c4a5fff0c49bb72c9c4d5f6c68faf69
    --
    -- But that lead to the generation of combinational loops. Now that we no
    -- longer traverse into recursive groups of let-bindings, the issue #1316
    -- that the above commit tried to solve, no longer shows up.
    substLifted isN substitution lifted = do
      -- remove the self-substitutions for the respective lifted expressions
      let subsMatrix = l2m substitution
      lifted1 <- Monad.zipWithM (\s (eL,seen) -> collectGlobals isN s seen eL)
                                 subsMatrix
                                 lifted
      return (map (^. _1) lifted1)

    l2m = go []
     where
      go _  []     = []
      go xs (y:ys) = (xs ++ ys) : go (xs ++ [y]) ys

disjointExpressionConsolidation _ e = return e
{-# SCC disjointExpressionConsolidation #-}

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

-- | Flatten's letrecs after `inlineCleanup`
--
-- `inlineCleanup` sometimes exposes additional possibilities for `caseCon`,
-- which then introduces let-bindings in what should be ANF. This transformation
-- flattens those nested let-bindings again.
--
-- NB: must only be called in the cleaning up phase.
flattenLet :: HasCallStack => NormRewrite
flattenLet (TransformContext is0 _) (Letrec binds body) = do
  let is1 = extendInScopeSetList is0 (map fst binds)
      bodyOccs = Lens.foldMapByOf
                   freeLocalIds (unionVarEnvWith (+))
                   emptyVarEnv (`unitVarEnv` (1 :: Int))
                   body
  (is2,binds1) <- second concat <$> List.mapAccumLM go is1 binds
  e1WorkFree <-
    case binds1 of
      [(_,e1)] -> isWorkFree e1
      _ -> pure (error "flattenLet: unreachable")
  case binds1 of
    -- inline binders into the body when there's only a single binder, and only
    -- if that binder doesn't perform any work or is only used once in the body
    [(id1,e1)] | Just occ <- lookupVarEnv id1 bodyOccs, e1WorkFree || occ < 2 ->
      if id1 `localIdOccursIn` e1
         -- Except when the binder is recursive!
         then return (Letrec binds1 body)
         else let subst = extendIdSubst (mkSubst is2) id1 e1
              in changed (substTm "flattenLet" subst body)
    _ -> return (Letrec binds1 body)
  where
    go :: InScopeSet -> LetBinding -> NormalizeSession (InScopeSet,[LetBinding])
    go isN (id1,collectTicks -> (Letrec binds1 body1,ticks)) = do
      let bs1 = map fst binds1
      let (binds2,body2,isN1) =
            -- We need to deshadow because we're merging nested let-expressions
            -- into a single let-expression: and within a let-expression, the
            -- bindings are not allowed to shadow each-other. Of course, we
            -- only need to deshadow if any shadowing is happening in the
            -- first place.
            --
            -- This is much better than blindly calling freshenTm, and saves
            -- almost 30% run-time of the normalization phase on some examples.
            if any (`elemInScopeSet` isN) bs1 then
              let Letrec bindsN bodyN = deShadowTerm isN (Letrec binds1 body1)
              in  (bindsN,bodyN,extendInScopeSetList isN (map fst bindsN))
            else
              (binds1,body1,extendInScopeSetList isN bs1)
      let bodyOccs = Lens.foldMapByOf
                       freeLocalIds (unionVarEnvWith (+))
                       emptyVarEnv (`unitVarEnv` (1 :: Int))
                       body2
          (srcTicks,nmTicks) = partitionTicks ticks
      e2WorkFree <-
        case binds2 of
          [(_,e2)] -> isWorkFree e2
          _ -> pure (error "flattenLet: unreachable")
      -- Distribute the name ticks of the let-expression over all the bindings
      (isN1,) . map (second (`mkTicks` nmTicks)) <$> case binds2 of
        -- inline binders into the body when there's only a single binder, and
        -- only if that binder doesn't perform any work or is only used once in
        -- the body
        [(id2,e2)] | Just occ <- lookupVarEnv id2 bodyOccs, e2WorkFree || occ < 2 ->
          if id2 `localIdOccursIn` e2
             -- Except when the binder is recursive!
             then changed ([(id2,e2),(id1, body2)])
             else let subst = extendIdSubst (mkSubst isN1) id2 e2
                  in  changed [(id1
                               -- Only apply srcTicks to the body
                               ,mkTicks (substTm "flattenLetGo" subst body2)
                                        srcTicks)]
        bs -> changed (bs ++ [(id1
                               -- Only apply srcTicks to the body
                              ,mkTicks body2 srcTicks)])
    go isN b = return (isN,[b])

flattenLet _ e = return e
{-# SCC flattenLet #-}

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
    Just (dc,argTys@(_:_:_)) ->
      let
        nm = mkDerivedName ctx (nameOcc (varName b))
        bs0 = map (`mkLocalId` nm) argTys
        (is1, bs1) = List.mapAccumL newBinder is0 bs0
        subst = extendIdSubst (mkSubst is1) b (mkApps dc (map (Left . Var) bs1))
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
      Just (_,argTys@(_:_:_)) -> do
        tmArgs <- mapM (mkSelectorCase ($(curLoc) ++ "splitArg") is0 tcm tmArg 1)
                       [0..length argTys - 1]
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
      (_, [])    -> changed (Prim (PrimInfo "Clash.XException.errorX" ty WorkConstant Nothing))
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

