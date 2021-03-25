{-|
Copyright   : (C) 2020-2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

This module provides the "evaluation" part of the partial evaluator. This
is implemented in the classic "eval/apply" style, with a variant of apply for
performing type applications.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PartialEval.Eval
  ( eval
  , forceEval
  , apply
  , applyTy
  ) where

import           Control.Exception (IOException)
import           Control.Monad (filterM, foldM, when, zipWithM)
import           Control.Monad.Catch hiding (mask)
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Either
import           Data.Graph (SCC(..))
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe (catMaybes)
import           Data.Primitive.ByteArray (ByteArray(..))
import qualified Data.Text as Text

#if MIN_VERSION_base(4,15,0)
import           GHC.Num.Integer (Integer (..))
#else
import           GHC.Integer.GMP.Internals (BigNat(..), Integer(..))
import           Clash.Core.TyCon (tyConDataCons)
import           Clash.Unique (lookupUniqMap)
#endif

import           GHC.Stack (HasCallStack)

#if MIN_VERSION_ghc(9,0,0)
import           GHC.Types.Basic (InlineSpec(..))
#else
import           BasicTypes (InlineSpec(..))
#endif

import           Clash.Core.DataCon (DataCon(..))
import           Clash.Core.EqSolver (isAbsurdPat, patEqs, solveNonAbsurds)
import           Clash.Core.HasFreeVars
import           Clash.Core.HasType
import           Clash.Core.Literal (Literal(..))
import           Clash.Core.Name (nameOcc)
import           Clash.Core.PartialEval.AsTerm
import           Clash.Core.PartialEval.Monad
import           Clash.Core.PartialEval.NormalForm
import           Clash.Core.Subst (deShadowAlt, deShadowTerm)
import           Clash.Core.Term
import           Clash.Core.TermInfo hiding (isFun)
import           Clash.Core.Type
import qualified Clash.Core.Util as Util
import           Clash.Core.Var
import           Clash.Core.VarEnv
import           Clash.Debug (debugIsOn, traceM)
import           Clash.Driver.Types (Binding(..), IsPrim(..))
import qualified Clash.Normalize.Primitives as NP (undefined)

import           Clash.GHC.PartialEval.Primitive.Info (resultType)

-- | Evaluate a term to WHNF.
--
eval :: (HasCallStack) => Term -> Eval Value
eval ticked = do
  let (term, ticks) = collectTicks ticked

  case term of
    Var i -> do
      result <- evalVar i
      pure (mkValueTicks result ticks)

    Literal lit -> do
      pure (mkValueTicks (VLiteral lit) ticks)

    Data dc -> do
      result <- evalData dc
      pure (mkValueTicks result ticks)

    Prim pr -> do
      result <- evalPrim pr
      pure (mkValueTicks result ticks)

    Lam i x -> do
      result <- evalLam i x
      pure (mkValueTicks result ticks)

    TyLam i x -> do
      result <- evalTyLam i x
      pure (mkValueTicks result ticks)

    App x y -> do
      result <- evalApp x (Left y)
      pure (retickResult result ticks)

    TyApp x ty -> do
      result <- evalApp x (Right ty)
      pure (retickResult result ticks)

    Letrec bs x -> do
      result <- evalLetrec bs x
      pure (mkValueTicks result ticks)

    Case x ty alts -> do
      result <- evalCase x ty alts
      pure (mkValueTicks result ticks)

    Cast x a b -> do
      result <- evalCast x a b
      pure (mkValueTicks result ticks)

    Tick _ _ -> error "eval: impossible case"

retickResult :: Value -> [TickInfo] -> Value
retickResult value ticks =
  case value of
    VNeutral (NeLetrec bs x) ->
      let bs' = fmap (\b -> mkValueTicks b ticks) <$> bs
          x'  = mkValueTicks x ticks
       in VNeutral (NeLetrec bs' x')

    _ -> mkValueTicks value ticks

forceEval :: (HasCallStack) => Value -> Eval Value
forceEval = forceEvalWith [] []

normTyBinders :: [(TyVar, Type)] -> Eval [(TyVar, Type)]
normTyBinders =
  let normTyBinder = bitraverse normVarTy normTy
   in traverse normTyBinder

forceEvalWith :: [(TyVar, Type)] -> [(Id, Value)] -> Value -> Eval Value
forceEvalWith tvs ids = \case
  VThunk term env -> do
    tvs' <- normTyBinders tvs
    setLocalEnv env (withTyVars tvs' . withIds ids $ eval term)

  -- A ticked thunk is still a thunk.
  VTick value tick ->
    flip VTick tick <$> forceEvalWith tvs ids value

  value -> pure value

forceArgs :: Args Value -> Eval (Args Value)
forceArgs =
  let forceArg = bitraverse forceEval normTy
   in traverse forceArg

{-
NOTE [strict primitives]
~~~~~~~~~~~~~~~~~~~~~~~~
When evaluating, we want to avoid duplication of work where it can be avoided.
However, sometimes avoiding inlining because a term performs work can lead to
suboptimal results from partial evaluation, like a term which can be constant
folded not being constant folded. One way we do this is by forcing inlining
of local variables when evaluating the subject of a case expression, but this
alone is still not sufficient.

As the environment delays substitutions, it can also appear that a primitive
which would reduce to a constant performs work, preventing it being inlined.
To prevent this, the evaluator always evaluates primitives strictly, meaning
that if they appear in a let binding or as an argument in application they are
reduced immediately instead of creating a thunk. This leads to more places
where let expressions can be removed entirely, and more places where
applications can be performed without needing to create a workArg binding.
-}

delayEval :: Term -> Eval Value
delayEval term =
  case fst (collectArgs term) of
    Prim{} -> eval term
    _ -> VThunk term <$> getLocalEnv

delayArgs :: Args Term -> Eval (Args Value)
delayArgs =
  let delayArg = bitraverse delayEval normTy
   in traverse delayArg

evalVar :: (HasCallStack) => Id -> Eval Value
evalVar i
  | isLocalId i = lookupLocal i
  | otherwise   = lookupGlobal i

-- An inlined term must be wrapped in a tick which prefixes names with the
-- name of the identifier which was inlined.
--
tickInlined :: Id -> Value -> Value
tickInlined i value =
  VTick value (NameMod PrefixName (LitTy $ SymTy (nameOf i)))
 where
  unQual = snd . Text.breakOnEnd "."
  nameOf = Text.unpack . unQual . nameOcc . varName

-- Test whether a value is eligable for inlining.
canInline :: Value -> Eval Bool
canInline value = do
  context <- getContext
  tcm <- getTyConMap

  let vTy = valueType tcm value
  let isClass = isClassTy tcm vTy

  -- TODO Does Primitive need a different rule
  case context of
    CaseSubject -> do
      expandable <- expandableValue value
      pure (isClass || expandable)

    _ -> do
      workFree <- workFreeValue value
      pure (isClass || workFree)
 where
  valueType tcm = termType tcm . unsafeAsTerm

lookupLocal :: Id -> Eval Value
lookupLocal i = do
  var <- normVarTy i
  val <- findId var

  case val of
    Just x -> do
      inlinable <- canInline x
      let isFun = isPolyFunTy (varType var)

      if isFun || inlinable
        then tickInlined var <$> forceEval x
        else pure (VNeutral (NeVar var))

    Nothing -> pure (VNeutral (NeVar var))

lookupGlobal :: Id -> Eval Value
lookupGlobal i = do
  fuel <- getFuel
  var <- findBinding i
  target <- getTarget

  if i == target then pure (VNeutral (NeVar i)) else
    case var of
      Just x
        -- The binding cannot be inlined. Note that this is limited to bindings
        -- which are not primitives in Clash, as these must be marked NOINLINE.
        |  bindingSpec x == NoInline
        ,  bindingIsPrim x == IsFun
        -> do -- traceM ("lookupGlobal(" <> showPpr i <> "): NOINLINE\n")
              pure (VNeutral (NeVar i))

        |  otherwise
        -> withTarget i $ do
              -- We check if the identifier is work free, otherwise isWorkFreeBinder
              -- is not used and we may decide a self-recursive binding is work free.
              inlinable <- canInline (VNeutral (NeVar i))
              fuel <- getFuel

              if | inlinable -> updateGlobal i x
                 | fuel > 0  -> withFuel (updateGlobal i x)
                 | otherwise -> pure (VNeutral (NeVar i))

      Nothing ->
        pure (VNeutral (NeVar i))
 where
  updateGlobal j x =
    withTarget j $ do
      value <- forceEval (bindingTerm x)
      replaceBinding (x { bindingTerm = value })
      pure (tickInlined j value)

evalData :: (HasCallStack) => DataCon -> Eval Value
evalData dc
  | fullyApplied (dcType dc) [] =
      VData dc [] <$> getLocalEnv

  | otherwise =
      etaExpand (Data dc) >>= eval

evalPrim :: (HasCallStack) => PrimInfo -> Eval Value
evalPrim pr
  | fullyApplied (primType pr) [] =
      evalPrimitive pr []

  | otherwise =
      etaExpand (Prim pr) >>= eval

-- | Evaluate a primitive with the given arguments.
-- See NOTE [Evaluating primitives] for more information.
--
evalPrimitive :: PrimInfo -> Args Value -> Eval Value
evalPrimitive pr args = do
  ty <- resultType pr args

  case HashMap.lookup (primName pr) primitives of
    Just f ->
      f pr args `catches`
        [ -- Catch an Eval specific error and attempt to correct it.
          -- TODO This should print warnings if Clash is built with +debug
          Handler $ \(e :: EvalException) ->
            case e of
              ResultUndefined ->
                eval (TyApp (Prim NP.undefined) ty)

              UnexpectedArgs pr' args' -> do
                when debugIsOn $
                  let failed  = show (primName pr') <> " with args:\n" <> show args'
                      context = show (primName pr) <> " with args:\n" <> show args
                   in traceM ("evalPrimitive: Unexpected arguments while evaluating " <> failed <> " from " <> context)

                forcedArgs <- forceArgs args
                pure (VNeutral (NePrim pr forcedArgs))

              _ -> do
                forcedArgs <- forceArgs args
                pure (VNeutral (NePrim pr forcedArgs))

          -- The Alternative / MonadPlus instance for IO throws an IOException
          -- on empty / mzero. Catch this and return a neutral primitive.
        , Handler $ \(_ :: IOException) -> do
            forcedArgs <- forceArgs args
            pure (VNeutral (NePrim pr forcedArgs))
        ]

    Nothing -> do
      when debugIsOn $ do
        let hasUnfolding = case primUnfolding pr of
                             Unfolding _ -> "has unfolding"
                             NoUnfolding -> "no unfolding"

        traceM ("evalPrimitive: " <> show (primName pr) <> ": no implementation, " <> hasUnfolding)

      forcedArgs <- forceArgs args
      pure (VNeutral (NePrim pr forcedArgs))
 where
  primitives = HashMap.empty

{-
NOTE [Evaluating primitives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the evaluator encounters a primitive operation with all arguments applied,
it will attempt to evaluate it. If this is possible, the call to the primitive
will be replaced with the result. However, it may not be possible to evaluate
a primitive if not all arguments are statically known (i.e. if an argument is
a variable with an unknown value). In this case, a neutral primitive is
returned instead.

Some primitives do not evaluate, and are deliberately preserved in the result
of the evaluator as neutral primitives. Notable examples of this are

  * GHC.CString.unpackCString#
  * Clash.Sized.Internal.BitVector.fromInteger##
  * Clash.Sized.Internal.BitVector.fromInteger#
  * Clash.Sized.Internal.Index.fromInteger#
  * Clash.Sized.Internal.Signed.fromInteger#
  * Clash.Sized.Internal.Unsigned.fromInteger#

Some primitives may throw exceptions (such as division by zero) or need to
perform IO (e.g. primitives on ByteArray#). These effects are supported by the
Eval monad, see Clash.Core.PartialEval.Monad.
-}

fullyApplied :: Type -> Args a -> Bool
fullyApplied ty args =
  length (fst $ splitFunForallTy ty) == length args

etaExpand :: Term -> Eval Term
etaExpand term = do
  tcm <- getTyConMap

  case collectArgs term of
    x@(Data dc, _) -> expand tcm (dcType dc) x
    x@(Prim pr, _) -> expand tcm (primType pr) x
    _ -> pure term
 where
  etaNamesOf acc = \case
    []        -> pure acc
    (x:xs)  ->
      case x of
        Left tv  -> etaNamesOf (Right tv : acc) xs
        Right ty -> do
          i <- getUniqueId "eta" ty
          withInScope i (etaNamesOf (Left i : acc) xs)

  expand tcm ty (tm, args) = do
    let (missingTys, _) = splitFunForallTy (applyTypeToArgs tm tcm ty args)
    missingArgs <- reverse <$> etaNamesOf [] missingTys

    pure $ mkAbstraction
      (mkApps term (fmap (bimap Var VarTy) missingArgs))
      missingArgs

evalLam :: (HasCallStack) => Id -> Term -> Eval Value
evalLam i x = do
  var <- normVarTy i
  withInScope var (VLam var x <$> getLocalEnv)

evalTyLam :: (HasCallStack) => TyVar -> Term -> Eval Value
evalTyLam i x = do
  var <- normVarTy i
  withInScope var (VTyLam var x <$> getLocalEnv)

evalApp :: (HasCallStack) => Term -> Arg Term -> Eval Value
evalApp x y
  | Data dc <- f
  , dcArgs  <- fst $ splitFunForallTy (dcType dc)
  , numArgs <- length dcArgs
  = case compare (length args) numArgs of
      -- The data constructor is under-applied, eta expand and evaluate the
      -- result.
      LT -> etaExpand term >>= eval

      -- The data constructor has all arguments given, and is a value.
      EQ -> do
        argThunks <- delayArgs args
        env <- getLocalEnv

        pure (VData dc argThunks env)

      -- The data constructor is over-applied. This can only be an error in
      -- the partial evaluator.
      GT -> error "evalApp: Overapplied data constructor"

  | Prim pr <- f
  , prArgs  <- fst $ splitFunForallTy (primType pr)
  , numArgs <- length prArgs
  = case compare (length args) numArgs of
      -- The primitive is under-applied, eta expand and evaluate the result.
      -- This will attempt primitive reducition with the eta-expanded version
      -- which may still reduce depending on the primitive.
      LT -> etaExpand term >>= eval

      -- The primitive has all arguments given, attempt primitive reduction.
      EQ -> do
        argThunks <- delayArgs args
        let tyVars = lefts prArgs
            tyArgs = rights args

        withTyVars (zip tyVars tyArgs) (evalPrimitive pr argThunks)

      -- The primitive is over-applied, i.e. it returns a function type after
      -- primitive reduction. Primitive reduction is performed, and the
      -- remaining arguments applied to the result.
      GT -> do
        let (pArgs, rArgs) = splitAt numArgs args
        pArgThunks <- delayArgs pArgs
        let tyVars = lefts prArgs
            tyArgs = rights args

        primRes <- withTyVars (zip tyVars tyArgs) (evalPrimitive pr pArgThunks)
        rArgThunks <- delayArgs rArgs
        foldM applyArg primRes rArgThunks

  | otherwise
  = do evalF <- eval f

       -- If the LHS of an application is undefined, the result can only be
       -- undefined, so there is no point evaluating the arguments.
       if isUndefined evalF
         then do
           tcm <- getTyConMap
           let resultTy = termType tcm term
           eval (TyApp (Prim NP.undefined) resultTy)
         else do
           argThunks <- delayArgs args
           foldM applyArg evalF argThunks
 where
  term = either (App x) (TyApp x) y
  (f, args) = collectArgs term

evalLetrec :: (HasCallStack) => [LetBinding] -> Term -> Eval Value
evalLetrec bs x = evalSccs x (Util.sccLetBindings bs)
 where
  -- Evaluate let bindings one by one according to their ordered SCCs. This is
  -- necessary to ensure that each let binding is delayed with an environment
  -- containing all previous bindings needed for evaluation.
  evalSccs body = \case
    [] -> eval body
    (scc:sccs) ->
      case scc of
        AcyclicSCC (i, b) -> do
          var <- normVarTy i
          val <- delayEval b
          rest <- withId var val (evalSccs body sccs)
          workFree <- workFreeValue val
          let nonSharable = isPolyFunTy (varType var)

          -- We keep let bindings which perform work, as it may not be possible
          -- to inline them during evaluation. Sometimes this is redundant, as
          -- the binding is only used once (and could be inlined) or is used
          -- as a case subject and would be removed from the final circuit.
          if nonSharable || workFree
            then pure rest
            else pure (VNeutral (NeLetrec [(var, val)] rest))

        CyclicSCC ibs -> do
          -- Each let binding in a recursive group must be delayed as a let
          -- expression with the other bindings in the group. This is because
          -- the evaluator is too strict to delay each binding with an
          -- environment containing each other binding (i.e. using mfix).
          let go (i, b) = do var <- normVarTy i
                             let ibs' = filter (\(j,_) -> var /= j) ibs
                             val <- if null ibs' then delayEval b else delayEval (Letrec ibs' b)

                             pure (var, val)

          binds <- traverse go ibs
          rest <- withIds binds (evalSccs body sccs)

          pure (VNeutral (NeLetrec binds rest))

evalCase :: (HasCallStack) => Term -> Type -> [Alt] -> Eval Value
evalCase term ty alts = do
  let altFvs = freeVarsOf (Case term ty alts)
  subject <- withContext CaseSubject (delayEval term)
  altTy <- normTy ty

  withInScopeList (eltsVarSet altFvs) $ do
    inScope <- getInScope

    -- Deshadow alts to prevent universal / existential collisions
    -- in recursive types (e.g. n and n1 for Vec)
    delayedAlts <- delayAlts (deShadowAlt inScope <$> alts)

    case delayedAlts of
      -- Case expressions of the form "case i[LocalId] of { _ -> e }" should
      -- force their argument to WHNF and update the local environment.
      [(DefaultPat, v)]
        |  Var i <- term
        -> do forced <- eval term
              withId i forced (forceEval v)

      -- Case expressions with one non-absurd alternative which binds no
      -- pattern variables can be replaced with just the alternative RHS.
      [(p, v)]
        | localVarsDoNotOccurIn (patVars p) (unsafeAsTerm v) -> forceEval v

      -- Other case expressions have to go through caseCon + tryTransformCase,
      -- no shortcuts can be taken in advance.
      _ ->
        -- Set the pattern vars as being in scope before evaluating the case.
        let bound = concatMap (patVars . fst) delayedAlts
         in withInScopeList bound (caseCon subject altTy delayedAlts)

-- | Attempt to apply the case-of-known-constructor transformation on a case
-- expression. If no suitable alternative can be chosen, attempt to transform
-- the case expression to try and expose more opportunities.
--
caseCon :: Value -> Type -> [(Pat, Value)] -> Eval Value
caseCon subject altTy alts = do
  forcedSubject <- withContext CaseSubject (forceEval subject)

  -- If the subject is undefined, the whole expression is undefined.
  if isUndefined forcedSubject then eval (TyApp (Prim NP.undefined) altTy) else
    case stripValue forcedSubject of
      -- Known literal: attempt to match or throw an error.
      VLiteral lit -> do
        let def = throwM (CannotMatch forcedSubject (fmap fst alts))
        match <- findBestAlt (matchLiteral lit) alts
        evalAlt def match

      -- Known data constructor: attempt to match or throw an error.
      -- The environment is not used matching does not need evaluation.
      VData dc args _ -> do
        let def = throwM (CannotMatch forcedSubject (fmap fst alts))
        match <- findBestAlt (matchData dc args) alts
        evalAlt def match

      -- Neutral primitives may be clash primitives which are treated as
      -- values, like fromInteger# for various types in clash-prelude.
      VNeutral (NePrim pr args) -> do
        let def = tryTransformCase forcedSubject altTy alts
        match <- findBestAlt (matchPrimLiteral pr args) alts
        evalAlt def match

      -- We know nothing: attempt case-of-case / case-of-let.
      _ -> tryTransformCase forcedSubject altTy alts

-- | Attempt to apply a transformation to a case expression to expose more
-- opportunities for caseCon. If no transformations can be applied the
-- case expression can only be neutral.
--
tryTransformCase :: Value -> Type -> [(Pat, Value)] -> Eval Value
tryTransformCase subject altTy alts =
  case stripValue subject of
    -- A case of case: pull out the inner case expression if possible and
    -- attempt caseCon on the new case expression.
    VNeutral (NeCase innerSubject _ innerAlts) -> do
      forcedInnerAlts <- forceAlts innerAlts

      if any (isKnown . snd) forcedInnerAlts then
        -- We can do case of case, attempt caseCon on the result
        let asCase v = VNeutral (NeCase v altTy alts)
            newAlts  = second asCase <$> forcedInnerAlts
         in caseCon innerSubject altTy newAlts

        -- We cannot do case of case, force alternatives.
      else do
        forcedAlts <- forceAlts alts
        pure (VNeutral (NeCase subject altTy forcedAlts))

    -- A case of let: Pull out the let expression if possible and attempt
    -- caseCon on the new case expression.
    VNeutral (NeLetrec bindings innerSubject) -> do
      newCase <- caseCon innerSubject altTy alts
      pure (VNeutral (NeLetrec bindings newCase))

    -- There is no way to continue evaluating the case, force all alternatives.
    _ -> do
      forcedAlts <- forceAlts alts
      pure (VNeutral (NeCase subject altTy forcedAlts))
 where
  -- We only care about case of case if alternatives of the inner case
  -- expression correspond to something we can do caseCon on.
  --
  isKnown = \case
    VNeutral (NePrim pr _) ->
      primName pr `elem`
        [ "Clash.Sized.Internal.BitVector.fromInteger##"
        , "Clash.Sized.Internal.BitVector.fromInteger#"
        , "Clash.Sized.Internal.Index.fromInteger#"
        , "Clash.Sized.Internal.Signed.fromInteger#"
        , "Clash.Sized.Internal.Unsigned.fromInteger#"
        ]

    VLiteral{} -> True
    VData{} -> True
    _ -> False

-- | For each pattern, solve existential variables and refine until either no
-- more existentials can be solved, or the pattern can be identified as absurd.
--
-- This corresponds to elimExistentials and caseElemNonReachable in the old
-- transformation pipeline.
--
solveAndElim :: [Pat] -> Eval [Maybe (Pat, [(TyVar, Type)])]
solveAndElim pats = do
  tcm <- getTyConMap
  traverse (go tcm []) pats
 where
  go tcm sols pat
    -- We obviously don't want to keep absurd patterns.
    | isAbsurdPat tcm pat =
        pure Nothing

    | otherwise =
        case pat of
          DataPat dc tvs ids ->
            case solveNonAbsurds tcm (mkVarSet tvs) (patEqs tcm pat) of
              -- No new solutions, the pattern cannot be refined further.
              [] -> pure (Just (pat, sols))

              -- New solutions are available, using these solutions may mean
              -- checking again yields more solutions.
              ss -> withTyVars ss $ do
                      ids' <- traverse normVarTy ids
                      go tcm (sols <> ss) (DataPat dc tvs ids')

          _ -> pure (Just (pat, sols))

-- Delay the evaluation of alternatives, eliminating any alternatives
-- immediately if they can be shown to be absurd by 'solveAndElim'.
--
delayAlts :: [Alt] -> Eval [(Pat, Value)]
delayAlts (unzip -> (pats, terms)) = do
  normPats <- traverse normPat pats
  solvedPats <- solveAndElim normPats
  newAlts <- zipWithM goAlts solvedPats terms

  pure (catMaybes newAlts)
 where
  normPat = \case
    DataPat dc tvs ids -> do
      -- We still need to normalize the tyvars and ids with the types in scope
      -- before we try to use solveAndElim. If we skip this, solveAndElim may
      -- not be able to solve anything at all, and absurd alts won't be removed.
      tvs' <- traverse normVarTy tvs
      let tys = fmap (\tv -> (tv, VarTy tv)) tvs'
      ids' <- withTyVars tys (traverse normVarTy ids)

      pure (DataPat dc tvs' ids')

    pat -> pure pat

  goAlts patSols term =
    case patSols of
      Nothing -> pure Nothing
      Just (pat, tys) ->
        withTyVars tys $ do
          -- We always need to delay primitives in case alternatives, as they
          -- may require variables bound in patterns to evaluate correctly.
          value <- VThunk term <$> getLocalEnv
          pure (Just (pat, value))

forceAlts :: [(Pat, Value)] -> Eval [(Pat, Value)]
forceAlts = traverse (bitraverse pure forceEval)

data PatResult
  = Match   (Pat, Value) [(TyVar, Type)] [(Id, Value)]
  | NoMatch
  deriving (Show)

evalAlt :: Eval Value -> PatResult -> Eval Value
evalAlt def = \case
  Match (_, val) tvs ids -> do
    tvs' <- traverse (bitraverse normVarTy pure) tvs
    ids' <- withTyVars tvs' (traverse (bitraverse normVarTy pure) ids)
    body <- forceEvalWith tvs' ids' val

    -- Let bind any ids bound by the pattern which perform work. If these
    -- are used in the body, then they will not be inlined during evaluation so
    -- if not let bound they would appear as free variables in the result.

    workIds <- filterM (fmap not . mustInline) ids'

    case workIds of
      [] -> pure body
      _  -> pure (VNeutral (NeLetrec workIds body))

  NoMatch -> def
 where
  mustInline (_, value) = do
    workFree <- workFreeValue value
    expandable <- expandableValue value
    tcm <- getTyConMap

    let valTy = termType tcm (unsafeAsTerm value)
        isClass = isClassTy tcm valTy
        isFun   = isPolyFunTy valTy

    pure (isClass || isFun || (workFree && expandable))

matchLiteral :: Literal -> (Pat, Value) -> Eval PatResult
matchLiteral lit alt@(pat, _) =
  case pat of
    DataPat dc [] [i]
      |  IntegerLiteral n <- lit
      -> case n of
#if MIN_VERSION_base(4,15,0)
           IS _
#else
           S# _
#endif
             | dcTag dc == 1 -> pure $ Match alt [] [(i, VLiteral (IntLiteral n))]

#if MIN_VERSION_base(4,15,0)
           IP bn
#else
           Jp# bn
#endif
             | dcTag dc == 2 -> matchBigNat i bn

#if MIN_VERSION_base(4,15,0)
           IN bn
#else
           Jn# bn
#endif
             | dcTag dc == 3 -> matchBigNat i bn

           _ -> pure NoMatch

      |  NaturalLiteral n <- lit
      -> case n of
#if MIN_VERSION_base(4,15,0)
           IS _
#else
           S# _
#endif
             | dcTag dc == 1 -> pure $ Match alt [] [(i, VLiteral (WordLiteral n))]

#if MIN_VERSION_base(4,15,0)
           IP bn
#else
           Jp# bn
#endif
             | dcTag dc == 2 -> matchBigNat i bn

           _ -> pure NoMatch

    LitPat n
      | lit == n -> pure $ Match alt [] []

    DefaultPat -> pure $ Match alt [] []

    _ -> pure NoMatch
 where
#if MIN_VERSION_base(4,15,0)
  matchBigNat i ba = do
    pure (Match alt [] [(i, VLiteral $ ByteArrayLiteral (ByteArray ba))])
#else
  -- This function is a nasty hack. We want the data constructor BN# from
  -- BigNat, but according to the TyConMap there are no constructors for
  -- Integer or Natural, which is where we would find BigNat. However, if you
  -- use integer-gmp, BigNat is in the TyConMap...
  matchBigNat i (BN# ba) = do
    tcm <- getTyConMap
    let Just bigNatTc = lookupUniqMap @Int 8214565720323826339 tcm
        [bnDc] = tyConDataCons bigNatTc
        arr = ByteArrayLiteral (ByteArray ba)

    val <- VData bnDc [Left (VLiteral arr)] <$> getLocalEnv
    pure (Match alt [] [(i, val)])
#endif

matchData :: DataCon -> Args Value -> (Pat, Value) -> Eval PatResult
matchData dc args alt@(pat, _) =
  case pat of
    DataPat c tvs ids
      |  dc == c
      -> let (tmArgs, tyArgs) = partitionEithers args
             tms = zip ids tmArgs
             tys = zip tvs (drop (length tyArgs - length tvs) tyArgs)
          in pure (Match alt tys tms)

    DefaultPat -> pure (Match alt [] [])
    _ -> pure NoMatch

{-
NOTE [matching primtives]
~~~~~~~~~~~~~~~~~~~~~~~~~
In both GHC and Clash, there exist primitives which can refer to either a data
constructor or a literal. For instance

  * W32# is a primitive, but also corresponds to a data constructor
  * Signed.fromInteger# is a primitive, but also corresponds to a literal

We keep these as primitives in the partial evalautor, as turning W32# into a
data constructor means it will not be turned into a primitive without another
pass over the AST. For both examples above, not leaving the value as as
primitive in the normalized AST result in failures later in compilation.
-}

-- TODO Should this also consider DataPat and data constructors?
-- The old evaluator did not, but matchData wouldn't cover it.
--
matchPrimLiteral :: PrimInfo -> Args Value -> (Pat, Value) -> Eval PatResult
matchPrimLiteral pr args alt@(pat, _) =
  case pat of
    DataPat dc [] [x]
      -- Char data constructor
      |  nameOcc (dcName dc) == "GHC.Types.C#"
      ,  [Left val] <- args
      -> do lit <- forceEval val

            case lit of
              VLiteral (CharLiteral _) -> pure (Match alt [] [(x, lit)])
              _ -> pure NoMatch

      -- Int data constructors
      |  name <- nameOcc (dcName dc)
      ,  [Left val] <- args
      ,  name `elem` ghcIntPrims
      -> do lit <- forceEval val

            case lit of
              VLiteral (IntLiteral _) -> pure (Match alt [] [(x, lit)])

              -- GHC.Int.Int64 is a special case
              VLiteral (Int64Literal _)
                |  name == "GHC.Int.I64#"
                -> pure (Match alt [] [(x, lit)])

              _ -> pure NoMatch

      -- Word data constructors
      |  name <- nameOcc (dcName dc)
      ,  [Left val] <- args
      , name `elem` ghcWordPrims
      -> do lit <- forceEval val

            case lit of
              VLiteral (WordLiteral _) -> pure (Match alt [] [(x, lit)])

              -- GHC.Word.Word64 is a special case
              VLiteral (Word64Literal _)
                |  name == "GHC.Word.W64#"
                -> pure (Match alt [] [(x, lit)])

              _ -> pure NoMatch

      -- Float data constructor
      |  nameOcc (dcName dc) == "GHC.Types.F#"
      ,  [Left val] <- args
      -> do lit <- forceEval val

            case lit of
              VLiteral (FloatLiteral _) -> pure (Match alt [] [(x, lit)])
              _ -> pure NoMatch

      -- Double data constructor
      |  nameOcc (dcName dc) == "GHC.Types.D#"
      ,  [Left val] <- args
      -> do lit <- forceEval val

            case lit of
              VLiteral (DoubleLiteral _) -> pure (Match alt [] [(x, lit)])
              _ -> pure NoMatch

    LitPat lit
      -- Bit literals
      |  primName pr == "Clash.Sized.BitVector.fromInteger##"
      ,  [Left mask, Left val] <- args
      -> do VLiteral (WordLiteral m) <- forceEval mask
            VLiteral l <- forceEval val

            if m == 0 && l == lit
              then pure (Match alt [] [])
              else pure NoMatch

      -- BitVector literals
      |  primName pr == "Clash.Sized.BitVector.fromInteger#"
      ,  [Right _, Left _, Left mask, Left val] <- args
      -> do VLiteral (NaturalLiteral m) <- forceEval mask
            VLiteral l <- forceEval val

            if m == 0 && l == lit
              then pure (Match alt [] [])
              else pure NoMatch

      -- Index / Sized / Unsigned literals
      |  primName pr `elem` clashSizedNumbers
      ,  [Right _, Left _, Left val] <- args
      -> do VLiteral l <- forceEval val

            if l == lit
              then pure (Match alt [] [])
              else pure NoMatch

    -- The primitive is not a special data constructor / literal
    _ -> pure NoMatch
 where
  ghcIntPrims =
    [ "GHC.Int.I8#"
    , "GHC.Int.I16#"
    , "GHC.Int.I32#"
    , "GHC.Int.I64"
    , "GHC.Types.I#"
    ]

  ghcWordPrims =
    [ "GHC.Word.W8#"
    , "GHC.Word.W16#"
    , "GHC.Word.W32#"
    , "GHC.Word.W64#"
    , "GHC.Types.W#"
    ]

  clashSizedNumbers =
    [ "Clash.Sized.Internal.Index.fromInteger#"
    , "Clash.Sized.Internal.Signed.fromInteger#"
    , "Clash.Sized.Internal.Unsigned.fromInteger#"
    ]

-- | Given a predicate to check if an alternative is a match, find the best
-- alternative that matches the predicate. Best is defined as being the most
-- specific matching pattern (meaning DefaultPat is only used if no other
-- pattern tried matches).
--
findBestAlt
  :: ((Pat, Value) -> Eval PatResult)
  -> [(Pat, Value)]
  -> Eval PatResult
findBestAlt checkAlt =
  go NoMatch
 where
  go acc [] = pure acc
  go acc (a:as) = do
    match <- checkAlt a
    case match of
      Match (pat, _) _ _
        | pat == DefaultPat -> go match as
        | otherwise -> pure match

      NoMatch -> go acc as

evalCast :: (HasCallStack) => Term -> Type -> Type -> Eval Value
evalCast x a b = VCast <$> eval x <*> normTy a <*> normTy b

applyArg :: Value -> Arg Value -> Eval Value
applyArg val =
  either (apply val) (applyTy val)

canApply :: Value -> Eval Bool
canApply value = do
  tcm <- getTyConMap

  let ty = valueType tcm value
      isClass = isClassTy tcm ty
      isFun = isPolyFunTy ty

  workFree <- workFreeValue value
  expandable <- expandableValue value

  pure (isClass || isFun || (workFree && expandable))
 where
  valueType tcm = termType tcm . unsafeAsTerm

apply :: (HasCallStack) => Value -> Value -> Eval Value
apply val arg = do
  tcm <- getTyConMap
  forced <- forceEval val
  applicable <- canApply arg

  let argTy = termType tcm (unsafeAsTerm arg)
  let (lhs, ticks) = collectValueTicks forced

  case lhs of
    -- If the LHS of application evaluates to a letrec, then add any bindings
    -- that do work to this letrec instead of creating a new one.
    VNeutral (NeLetrec bs x)
      | applicable -> do
          inner <- apply x arg
          pure (mkValueTicks (VNeutral (NeLetrec bs inner)) ticks)

      | otherwise -> do
          let bound = fmap fst bs

          withInScopeList bound $ do
            varTy <- normTy argTy
            var <- getUniqueId "workArg" varTy

            withInScope var $ do
              inner <- apply x (VNeutral (NeVar var))
              pure (mkValueTicks (VNeutral (NeLetrec (bs <> [(var, arg)]) inner)) ticks)

    -- If the LHS of application is neutral, make a letrec around the neutral
    -- application if the argument performs work.
    VNeutral neu
      | applicable  ->
          pure (mkValueTicks (VNeutral (NeApp neu arg)) ticks)

      | otherwise -> do
          varTy <- normTy argTy
          var <- getUniqueId "workArg" varTy

          withInScope var $ do
            let inner = VNeutral (NeApp neu (VNeutral (NeVar var)))
            pure (mkValueTicks (VNeutral (NeLetrec [(var, arg)] inner)) ticks)

    -- If the LHS of application is a lambda, make a letrec with the name of
    -- the argument around the result of evaluation if it performs work.
    VLam i x env
      | applicable -> do
          var <- normVarTy i

          setLocalEnv env $ do
            inner <- withId var arg (eval x)
            pure (mkValueTicks inner ticks)

      | otherwise ->
          setLocalEnv env $ do
            -- We rename i to j and bind the argument to j. This is somewhat of
            -- a hack to stop recursive functions with work performing arguments
            -- from binding let x = x in ... for arguments in recursive calls.
            j <- getUniqueId (nameOcc (varName i)) (varType i)
            let jVal = VNeutral (NeVar j)

            inScope <- getInScope
            let x' = deShadowTerm inScope x

            -- (j, arg) is not bound in the environment, as this leads to extra
            -- inlining with case subjects which currently leads to free FVs.
            inner <- withId i jVal (eval x')

            -- TODO Make this more efficient: only generate a single let for a
            -- work free argument that doesn't change between calls.
            pure (mkValueTicks (VNeutral (NeLetrec [(j, arg)] inner)) ticks)

    _ ->
      throwM (CannotApply lhs (Left arg))

applyTy :: (HasCallStack) => Value -> Type -> Eval Value
applyTy val ty = do
  forced <- forceEval val
  argTy <- normTy ty

  let (lhs, ticks) = collectValueTicks forced

  case lhs of
    VNeutral neu ->
      pure (mkValueTicks (VNeutral (NeTyApp neu argTy)) ticks)

    VTyLam i x env -> do
      var <- normVarTy i

      setLocalEnv env $ do
        inner <- withTyVar var argTy (eval x)
        pure (mkValueTicks inner ticks)

    _ -> throwM (CannotApply lhs (Right argTy))
