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
#if MIN_VERSION_base(4,15,0)
import           GHC.Num.Integer (Integer (..))
#else
import           GHC.Integer.GMP.Internals (BigNat(..), Integer(..))
#endif

#if MIN_VERSION_ghc(9,0,0)
import           GHC.Types.Basic (InlineSpec(..))
#else
import           BasicTypes (InlineSpec(..))
#endif

import           Clash.Core.DataCon (DataCon(..))
import           Clash.Core.HasType
import           Clash.Core.Literal (Literal(..))
import           Clash.Core.PartialEval.AsTerm
import           Clash.Core.PartialEval.Monad
import           Clash.Core.PartialEval.NormalForm
import           Clash.Core.Term
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
eval :: Term -> Eval Value
eval = \case
  Var i           -> evalVar i
  Literal lit     -> pure (VLiteral lit)
  Data dc         -> evalData dc
  Prim pr         -> evalPrim pr
  Lam i x         -> evalLam i x
  TyLam i x       -> evalTyLam i x
  App x y         -> evalApp x (Left y)
  TyApp x ty      -> evalApp x (Right ty)
  Letrec bs x     -> evalLetrec bs x
  Case x ty alts  -> evalCase x ty alts
  Cast x a b      -> evalCast x a b
  Tick tick x     -> evalTick tick x

delayEval :: Term -> Eval Value
delayEval = \case
  Literal lit -> pure (VLiteral lit)
  Lam i x -> evalLam i x
  TyLam i x -> evalTyLam i x
  Tick t x -> flip VTick t <$> delayEval x
  term -> VThunk term <$> getLocalEnv

forceEval :: Value -> Eval Value
forceEval = forceEvalWith [] []

forceEvalWith :: [(TyVar, Type)] -> [(Id, Value)] -> Value -> Eval Value
forceEvalWith tvs ids = \case
  VThunk term env -> do
    tvs' <- traverse (bitraverse pure normTy) tvs
    setLocalEnv env (withTyVars tvs' . withIds ids $ eval term)

  value -> pure value

delayArg :: Arg Term -> Eval (Arg Value)
delayArg = bitraverse delayEval normTy

delayArgs :: Args Term -> Eval (Args Value)
delayArgs = traverse delayArg

evalVar :: Id -> Eval Value
evalVar i
  | isLocalId i = lookupLocal i
  | otherwise   = lookupGlobal i

lookupLocal :: Id -> Eval Value
lookupLocal i = do
  var <- normVarTy i
  val <- findId var

  case val of
    Just x -> do
      workFree <- workFreeValue x
      if workFree then forceEval x else pure (VNeutral (NeVar var))

    Nothing -> pure (VNeutral (NeVar var))

lookupGlobal :: Id -> Eval Value
lookupGlobal i = do
  -- inScope <- getInScope
  fuel <- getFuel
  var <- findBinding i

  case var of
    Just x
      -- The binding cannot be inlined. Note that this is limited to bindings
      -- which are not primitives in Clash, as these must be marked NOINLINE.
      |  bindingSpec x == NoInline
      ,  bindingIsPrim x == IsFun
      -> pure (VNeutral (NeVar i))

      -- There is no fuel, meaning no more inlining can occur.
      |  fuel == 0
      -> pure (VNeutral (NeVar i))

      -- Inlining can occur, using one unit of fuel in the process.
      |  otherwise
      -> withContext i . withFuel $ do
           val <- forceEval (bindingTerm x)
           replaceBinding (x { bindingTerm = val })
           pure val

    Nothing
      -> pure (VNeutral (NeVar i))

evalData :: DataCon -> Eval Value
evalData dc
  | fullyApplied (dcType dc) [] =
      VData dc [] <$> getLocalEnv

  | otherwise =
      etaExpand (Data dc) >>= eval

evalPrim :: PrimInfo -> Eval Value
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
  etaNameOf =
    either (pure . Right) (fmap Left . getUniqueId "eta")

  expand tcm ty (tm, args) = do
    let (missingTys, _) = splitFunForallTy (applyTypeToArgs tm tcm ty args)
    missingArgs <- traverse etaNameOf missingTys

    pure $ mkAbstraction
      (mkApps term (fmap (bimap Var VarTy) missingArgs))
      missingArgs

evalLam :: Id -> Term -> Eval Value
evalLam i x = do
  env <- getLocalEnv
  var <- normVarTy i

  pure (VLam var x env)

evalTyLam :: TyVar -> Term -> Eval Value
evalTyLam i x = do
  env <- getLocalEnv
  var <- normVarTy i

  pure (VTyLam var x env)

evalApp :: Term -> Arg Term -> Eval Value
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

evalLetrec :: [LetBinding] -> Term -> Eval Value
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

evalCase :: Term -> Type -> [Alt] -> Eval Value
evalCase term ty as = do
  subject <- delayEval term
  resTy <- normTy ty
  alts <- delayAlts as

  caseCon subject resTy alts

-- | Attempt to apply the case-of-known-constructor transformation on a case
-- expression. If no suitable alternative can be chosen, attempt to transform
-- the case expression to try and expose more opportunities.
--
caseCon :: Value -> Type -> [(Pat, Value)] -> Eval Value
caseCon subject ty alts = do
  forcedSubject <- keepLifted (forceEval subject)

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

evalCast :: Term -> Type -> Type -> Eval Value
evalCast x a b = VCast <$> eval x <*> normTy a <*> normTy b

evalTick :: TickInfo -> Term -> Eval Value
evalTick tick x = VTick <$> eval x <*> pure tick

applyArg :: Value -> Arg Value -> Eval Value
applyArg val =
  either (apply val) (applyTy val)

apply :: Value -> Value -> Eval Value
apply val arg = do
  tcm <- getTyConMap
  forced <- forceEval val
  canApply <- workFreeValue arg
  let argTy = valueType tcm arg

  case stripValue forced of
    -- If the LHS of application evaluates to a letrec, then add any bindings
    -- that do work to this letrec instead of creating a new one.
    VNeutral (NeLetrec bs x)
      | canApply  -> do
          inner <- apply x arg
          pure (VNeutral (NeLetrec bs inner))

      | otherwise -> do
          varTy <- normTy argTy
          var <- getUniqueId "workArg" varTy
          inner <- apply x (VNeutral (NeVar var))
          pure (VNeutral (NeLetrec ((var, arg) : bs) inner))

    -- If the LHS of application is neutral, make a letrec around the neutral
    -- application if the argument performs work.
    VNeutral neu
      | canApply  -> pure (VNeutral (NeApp neu arg))
      | otherwise -> do
          varTy <- normTy argTy
          var <- getUniqueId "workArg" varTy
          let inner = VNeutral (NeApp neu (VNeutral (NeVar var)))
          pure (VNeutral (NeLetrec [(var, arg)] inner))

    -- If the LHS of application is a lambda, make a letrec with the name of
    -- the argument around the result of evaluation if it performs work.
    VLam i x env
      | canApply  -> setLocalEnv env $ withId i arg (eval x)
      | otherwise -> setLocalEnv env $ do
          inner <- withId i arg (eval x)
          pure (VNeutral (NeLetrec [(i, arg)] inner))

    f ->
      error ("apply: Cannot apply " <> show arg <> " to " <> show f)
 where
  -- TODO Write an instance for InferType Value and use that instead
  valueType tcm = inferCoreTypeOf tcm . asTerm

applyTy :: Value -> Type -> Eval Value
applyTy val ty = do
  forced <- forceEval val
  argTy <- normTy ty

  case stripValue forced of
    VNeutral n ->
      pure (VNeutral (NeTyApp n argTy))

    VTyLam i x env ->
      setLocalEnv env $ withTyVar i argTy (eval x)

    f ->
      error ("applyTy: Cannot apply " <> show argTy <> " to " <> show f)
