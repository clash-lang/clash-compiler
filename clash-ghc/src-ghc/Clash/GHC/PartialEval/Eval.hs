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
  , apply
  , applyTy
  ) where

import           Control.Monad (foldM)
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Either
import           Data.Graph (SCC(..))
import           Data.Primitive.ByteArray (ByteArray(..))
import qualified Data.Text as Text

#if MIN_VERSION_base(4,15,0)
import           GHC.Num.Integer (Integer (..))
#else
import           GHC.Integer.GMP.Internals (BigNat(..), Integer(..))
#endif

import           GHC.Stack (HasCallStack)

#if MIN_VERSION_ghc(9,0,0)
import           GHC.Types.Basic (InlineSpec(..))
#else
import           BasicTypes (InlineSpec(..))
#endif

import           Clash.Core.DataCon (DataCon(..))
import           Clash.Core.HasType
import           Clash.Core.Literal (Literal(..))
import           Clash.Core.Name (nameOcc)
import           Clash.Core.PartialEval.AsTerm
import           Clash.Core.PartialEval.Monad
import           Clash.Core.PartialEval.NormalForm
import           Clash.Core.Subst (deShadowTerm)
import           Clash.Core.Term
import           Clash.Core.TermInfo hiding (isFun)
import           Clash.Core.TyCon (tyConDataCons)
import           Clash.Core.Type
import           Clash.Core.TysPrim (integerPrimTy)
import qualified Clash.Core.Util as Util
import           Clash.Core.Var
import           Clash.Driver.Types (Binding(..), IsPrim(..))
import qualified Clash.Normalize.Primitives as NP (undefined)
import           Clash.Unique (lookupUniqMap')

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

forceEvalWith :: [(TyVar, Type)] -> [(Id, Value)] -> Value -> Eval Value
forceEvalWith tvs ids = \case
  VThunk term env -> do
    tvs' <- traverse (bitraverse pure normTy) tvs
    setLocalEnv env (withTyVars tvs' . withIds ids $ eval term)

  -- A ticked thunk is still a thunk.
  VTick value tick ->
    flip VTick tick <$> forceEvalWith tvs ids value

  value -> pure value

delayEval :: Term -> Eval Value
delayEval term =
  case fst (collectArgs term) of
    Prim{} -> eval term
    _ -> VThunk term <$> getLocalEnv

delayArg :: Arg Term -> Eval (Arg Value)
delayArg = bitraverse delayEval normTy

delayArgs :: Args Term -> Eval (Args Value)
delayArgs = traverse delayArg

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
  -- inScope <- getInScope
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
      evalPrimOp pr []

  | otherwise =
      etaExpand (Prim pr) >>= eval

-- TODO Hook up to primitive evaluation skeleton
evalPrimOp :: PrimInfo -> Args Value -> Eval Value
evalPrimOp pr args = pure (VNeutral (NePrim pr args))

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
  = if fullyApplied (dcType dc) args
      then do
        argThunks <- delayArgs args
        VData dc argThunks <$> getLocalEnv

      else etaExpand term >>= eval

  | Prim pr <- f
  , prArgs  <- fst $ splitFunForallTy (primType pr)
  , numArgs <- length prArgs
  = case compare (length args) numArgs of
      LT ->
        etaExpand term >>= eval

      EQ -> do
        argThunks <- delayArgs args
        let tyVars = lefts prArgs
            tyArgs = rights args

        withTyVars (zip tyVars tyArgs) (evalPrimOp pr argThunks)

      GT -> do
        let (pArgs, rArgs) = splitAt numArgs args
        pArgThunks <- delayArgs pArgs
        primRes <- evalPrimOp pr pArgThunks
        rArgThunks <- delayArgs rArgs

        foldM applyArg primRes rArgThunks

  | otherwise
  = preserveFuel $ do
      evalF <- eval f
      argThunks <- delayArgs args
      foldM applyArg evalF argThunks
 where
  term = either (App x) (TyApp x) y
  (f, args, _ticks) = collectArgsTicks term

evalLetrec :: (HasCallStack) => [LetBinding] -> Term -> Eval Value
evalLetrec bs x = do
  -- Determine if a binding should be kept in a letrec or inlined. We keep
  -- bindings which perform work to prevent duplication of registers etc.
  (keep, inline) <- foldM evalScc ([], []) (Util.sccLetBindings bs)
  eX <- withIds (keep <> inline) (eval x)

  case keep of
    [] -> pure eX
    _  -> pure (VNeutral (NeLetrec keep eX))
 where
  evalBind (i, y) = do
    var <- normVarTy i
    eY <- delayEval y

    pure (var, eY)

  evalScc (k, i) = \case
    AcyclicSCC y -> do
      eY <- evalBind y
      workFree <- workFreeValue (snd eY)

      if workFree then pure (k, eY:i) else pure (eY:k, i)

    CyclicSCC ys -> do
      eYs <- traverse evalBind ys
      pure (eYs <> k, i)

evalCase :: (HasCallStack) => Term -> Type -> [Alt] -> Eval Value
evalCase term ty as = do
  subject <- withContext CaseSubject (delayEval term)
  resTy <- normTy ty
  alts <- delayAlts as

  caseCon subject resTy alts

-- | Attempt to apply the case-of-known-constructor transformation on a case
-- expression. If no suitable alternative can be chosen, attempt to transform
-- the case expression to try and expose more opportunities.
--
caseCon :: Value -> Type -> [(Pat, Value)] -> Eval Value
caseCon subject ty alts = do
  forcedSubject <- withContext CaseSubject (forceEval subject)

  -- If the subject is undefined, the whole expression is undefined.
  case isUndefined forcedSubject of
    True -> eval (TyApp (Prim NP.undefined) ty)
    False ->
      case stripValue forcedSubject of
        -- Known literal: attempt to match or throw an error.
        VLiteral lit -> do
          let def = error ("caseCon: No pattern matched " <> show lit <> " in " <> show alts)
          match <- findBestAlt (matchLiteral lit) alts
          evalAlt def match

        -- Known data constructor: attempt to match or throw an error.
        -- The environment here is the same as the current environment.
        VData dc args _env -> do
          let def = error ("caseCon: No pattern matched " <> show dc <> " in " <> show alts)
          match <- findBestAlt (matchData dc args) alts
          evalAlt def match

        -- Neutral primitives may be clash primitives which are treated as
        -- values, like fromInteger# for various types in clash-prelude.
        VNeutral (NePrim pr args) -> do
          let def = VNeutral (NeCase forcedSubject ty alts)
          match <- findBestAlt (matchClashPrim pr args) alts
          evalAlt def match

        -- We know nothing: attempt case-of-case / case-of-let.
        _ -> tryTransformCase forcedSubject ty alts

-- | Attempt to apply a transformation to a case expression to expose more
-- opportunities for caseCon. If no transformations can be applied the
-- case expression can only be neutral.
--
tryTransformCase :: Value -> Type -> [(Pat, Value)] -> Eval Value
tryTransformCase subject ty alts =
  case stripValue subject of
    -- A case of case: pull out the inner case expression if possible and
    -- attempt caseCon on the new case expression.
    VNeutral (NeCase innerSubject _ innerAlts) -> do
      forcedAlts <- forceAlts innerAlts

      if all (isKnown . snd) forcedAlts
       then let asCase v = VNeutral (NeCase v ty alts)
                newAlts  = second asCase <$> innerAlts
             in caseCon innerSubject ty newAlts

        else pure (VNeutral (NeCase subject ty alts))

    -- A case of let: Pull out the let expression if possible and attempt
    -- caseCon on the new case expression.
    VNeutral (NeLetrec bindings innerSubject) -> do
      newCase <- caseCon innerSubject ty alts
      pure (VNeutral (NeLetrec bindings newCase))

    -- There is no way to continue evaluating the case, do nothing.
    -- TODO elimExistentials here.
    _ -> pure (VNeutral (NeCase subject ty alts))
 where
  -- We only care about case of case if alternatives of the inner case
  -- expression correspond to something we can do caseCon on.
  --
  -- TODO We may also care if it is another case of case?
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

delayAlts :: [Alt] -> Eval [(Pat, Value)]
delayAlts = traverse (bitraverse delayPat delayEval)
 where
  delayPat = \case
    DataPat dc tvs ids -> do
      tvs' <- traverse normVarTy tvs
      let tys  = fmap (\tv -> (tv, VarTy tv)) tvs'

      ids' <- withTyVars tys (traverse normVarTy ids)

      pure (DataPat dc tvs' ids')

    pat -> pure pat

forceAlts :: [(Pat, Value)] -> Eval [(Pat, Value)]
forceAlts = traverse (traverse forceEval)

data PatResult
  = Match   (Pat, Value) [(TyVar, Type)] [(Id, Value)]
  | NoMatch

evalAlt :: Value -> PatResult -> Eval Value
evalAlt def = \case
  Match (_, val) tvs ids -> do
    tvs' <- traverse (bitraverse normVarTy pure) tvs
    ids' <- withTyVars tvs' (traverse (bitraverse normVarTy pure) ids)
    forceEvalWith tvs' ids' val

  NoMatch -> pure def

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
  -- Somewhat of a hack: We find the constructor for BigNat and apply a
  -- ByteArray literal made from the given ByteArray to it.
#if MIN_VERSION_base(4,15,0)
  matchBigNat i ba = do
#else
  matchBigNat i (BN# ba) = do
#endif
    tcm <- getTyConMap
    let Just integerTcName = fmap fst (splitTyConAppM integerPrimTy)
        [_, jpDc, _] = tyConDataCons (lookupUniqMap' tcm integerTcName)
        ([bnTy], _) = splitFunTys tcm (dcType jpDc)
        Just bnTcName = fmap fst (splitTyConAppM bnTy)
        [bnDc] = tyConDataCons (lookupUniqMap' tcm bnTcName)

    let arr = ByteArrayLiteral (ByteArray ba)
    val <- VData bnDc [Left (VLiteral arr)] <$> getLocalEnv

    pure (Match alt [] [(i, val)])

matchData :: DataCon -> Args Value -> (Pat, Value) -> Eval PatResult
matchData dc args alt@(pat, _) =
  case pat of
    DataPat c tvs ids
      |  dc == c
      -> do let (tms, tys) = bimap (zip ids) (zip tvs) (partitionEithers args)
            pure (Match alt tys tms)

    DefaultPat -> pure (Match alt [] [])
    _ -> pure NoMatch

-- TODO Should this also consider DataPat and data constructors?
-- The old evaluator did not, but matchData wouldn't cover it.
--
matchClashPrim :: PrimInfo -> Args Value -> (Pat, Value) -> Eval PatResult
matchClashPrim pr args alt@(pat, _) =
  case pat of
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
      ,  [Right _n, Left _knN, Left mask, Left val] <- args
      -> do VLiteral (NaturalLiteral m) <- forceEval mask
            VLiteral l <- forceEval val

            if m == 0 && l == lit
              then pure (Match alt [] [])
              else pure NoMatch

      -- Sized integer / natural literals
      |  primName pr `elem` clashSizedNumbers
      ,  [Right _n, Left _knN, Left val] <- args
      -> do VLiteral l <- forceEval val

            if l == lit
              then pure (Match alt [] [])
              else pure NoMatch

    -- The primitive is not a literal from clash-prelude
    _ -> pure NoMatch
 where
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
  go !acc [] = pure acc
  go !acc (a:as) = do
    match <- checkAlt a
    case match of
      Match (pat, _term) _tvs _ids
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

    f ->
      error ("apply: Cannot apply " <> show arg <> " to " <> show f)

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

    f ->
      error ("applyTy: Cannot apply " <> show argTy <> " to " <> show f)
