{-|
  Copyright     : (C) 2020, QBayLogic B.V.
  License       : BSD2 (see the file LICENSE)
  Maintainer    : QBayLogic B.V. <devops@qbaylogic.com>

This module defines semantics for the partial evaluator that are common
to all compiler frontends, and exposes utility functions to create
new evaluator implementaions.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Core.Evaluator.Semantics where

import Control.Monad ((>=>), foldM)
import Data.Bifunctor (bimap, first)
import Data.Bitraversable (bitraverse)
import Data.List.Extra (equalLength)

-- TODO: This is GHC specific, but is already used in BindingMap.
import BasicTypes (InlineSpec(..))

import Clash.Core.DataCon
import Clash.Core.Evaluator.Models
import Clash.Core.Literal
import Clash.Core.Term
import Clash.Core.Termination
import Clash.Core.TermInfo
import Clash.Core.TyCon
import Clash.Core.Type
import Clash.Core.Var
import Clash.Driver.Types (Binding(..))

-- | Construct an evaluator given the three functions which are potentially
-- specific to different compiler front-ends:
--
--   * matchesLit, which checks if a literal is matched by a pattern
--   * matchesData, which checks if a constructor is matched by a pattern
--   * evalPrim, which evaluates primitive operations
--
evaluatorWith
  :: (Literal -> Pat -> Eval PatResult)
  -> (DataCon -> [Either Term Type] -> Pat -> Eval PatResult)
  -> (PrimInfo -> [Either Term Type] -> Eval Value)
  -> Evaluator
evaluatorWith matchLit matchData evalPrim =
  let eval  = evaluateWith matchLit matchData evalPrim
      quote = quoteWith eval
   in Evaluator eval quote

-- | Construct an evaluation function, eval :: Term -> Eval Value, from the
-- given functions. See 'evaluatorWith' for more information.
--
evaluateWith
  :: (Literal -> Pat -> Eval PatResult)
  -> (DataCon -> [Either Term Type] -> Pat -> Eval PatResult)
  -> (PrimInfo -> [Either Term Type] -> Eval Value)
  -> Term
  -> Eval Value
evaluateWith matchLit matchData evalPrim = go
 where
  apply = applyWith go

  go = \case
    Var v -> evaluateVarWith go v
    Literal l -> pure (VLit l)
    Data dc -> evaluateDataWith go dc
    Prim p -> evaluatePrimWith go evalPrim p
    Lam x e -> evaluateLam x e
    TyLam x e -> evaluateTyLam x e
    App x y -> evaluateAppWith go evalPrim apply x (Left y)
    TyApp x ty -> evaluateAppWith go evalPrim apply x (Right ty)
    Letrec bs x -> evaluateLetrecWith go bs x
    Case x ty xs -> evaluateCaseWith go matchLit matchData x ty xs
    Cast x a b -> evaluateCastWith go x a b
    Tick ti x -> evaluateTickWith go ti x

{-
Note [evaluating data and primitives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Evaluation of data constructors and primitives are handled in multiple parts
of this module:

  * evaluateDataWith (data constructors only)
  * evaluatePrimWith (primitives only)
  * evaluateAppWith  (both)

The reason for this is that data constructors and primitives are only
considered values when they are fully applied, so they include all arguments
when evaluated. When evaluating a term, constructors and primitives will appear
either on their own or nested in a sequence of App / TyApp nodes.

The special functions, evaluateDataWith and evalautePrimWith handle the first
case. As these are leaves in the term AST (they have no subterms), they are
only values if they take no arguments, otherwise they are eta-expanded, and
the newly created binders are used to provide variables for each argument.

To identify sequences of applications which are applied to constructors or
primitives, the evaluateAppWith function unwraps all applications. If it is
a constructor or primitive, it checks that all arguments are applied and
eta-expands if any are missing.

There is one more consideration which applies to primitives only: the result
of a primitive may potentially be a function (e.g. primtives that return an
action in IO). This is handled in evaluateAppWith, by checking if more
arguments are given than the primitive requires, and partitioning the
arguments. The arguments for the primitive are applied and the primitive is
evaluated, then the remaining arguments are applied.
-}

isFullyApplied :: Type -> [Either Term Type] -> Bool
isFullyApplied ty args =
  equalLength args (fst $ splitFunForallTy ty)

etaExpand :: Term -> Eval Term
etaExpand x =
  case collectArgs x of
    y@(Data dc, _) -> do
      tcm <- getTyConMap
      expand tcm (dcType dc) y

    y@(Prim p, _) -> do
      tcm <- getTyConMap
      expand tcm (primType p) y

    _ -> pure x
 where
  expand :: TyConMap -> Type -> (Term, [Either Term Type]) -> Eval Term
  expand tcm ty (tm, args) = do
    let missingArgTys = fst $ splitFunForallTy (applyTypeToArgs tm tcm ty args)
    missingArgs <- traverse etaNameOf missingArgTys

    pure $ mkAbstraction
      (mkApps x (fmap (bimap Var VarTy) missingArgs))
      missingArgs

  etaNameOf :: Either TyVar Type -> Eval (Either Id TyVar)
  etaNameOf = either (pure . Right) (fmap Left . mkUniqueId "eta")

-- | Default implementation for looking up a variable in the environment.
-- This checks both the local and global environments, and inlines global
-- binders which are not marked as NOINLINE.
--
-- As variables can point to unevalated terms or WHNF values, this function
-- also forces unevaluated terms to WHNF when performing lookups. This is
-- currently performed in an environment without the given identifier, to
-- prevent expressions like
--
--   let x = x in ...
--
-- looping if x is looked up, as it exists in the environment as
--
--   Var x |-> Var x
--
-- TODO: In the future this should not delete identifiers from the environment
-- if they are terminating. This will allow functions like fmap to be fully
-- unrolled where possible.
--
evaluateVarWith :: (Term -> Eval Value) -> Id -> Eval Value
evaluateVarWith eval i
  | isLocalId i = goLocal
  | otherwise   = goGlobal
 where
  goLocal :: Eval Value
  goLocal = getLocal i >>= \case
    Just x  -> forceLocal x
    Nothing -> pure (VNeu (NeVar i))
   where
    forceLocal = either (withoutLocal i . eval) pure

  goGlobal :: Eval Value
  goGlobal = do
    gRecInfo <- getRecInfo
    gFuel <- getFuel

    getGlobal i >>= \case
      Just b
        -- The binding can't be inlined.
        |  bindingSpec b == NoInline
        -> pure (VNeu (NeVar i))

        -- The binding can be inlined if there is enough fuel.
        |  isRecursive i gRecInfo
        -> if gFuel == 0
             then pure (VNeu (NeVar i))
             else do
               v <- forceGlobal (bindingTerm b)
               putFuel (gFuel - 1)
               updateGlobal i v
               pure v

        -- The binding can be inlined without using fuel.
        |  otherwise
        -> do v <- forceGlobal (bindingTerm b)
              updateGlobal i v
              pure v

      Nothing
        -> pure (VNeu (NeVar i))
   where
    forceGlobal = either eval pure

-- | Default implementation for evaluating a literal.
-- This simply wraps the literal up into a Value.
--
evaluateLiteral :: Literal -> Eval Value
evaluateLiteral = pure . VLit

-- | Default implementation for evaluating data constructors. If the
-- constructor is not nullary, then it is eta-expanded and its eta-expanded
-- form is evaluated to obtain a result.
--
evaluateDataWith
  :: (Term -> Eval Value)
  -> DataCon
  -> Eval Value
evaluateDataWith eval dc
  | isFullyApplied (dcType dc) [] = VData dc [] <$> getLocalEnv
  | otherwise = etaExpand (Data dc) >>= eval

-- | Default implementation for evaluating primitive operations. If the primop
-- is not nullary, then it is eta-expanded and its eta-expanded form is
-- evaluated with the primitive evaluator to obtain a result.
--
evaluatePrimWith
  :: (Term -> Eval Value)
  -> (PrimInfo -> [Either Term Type] -> Eval Value)
  -> PrimInfo
  -> Eval Value
evaluatePrimWith eval evalPrim p
  | isFullyApplied (primType p) [] = evalPrim p []
  | otherwise = etaExpand (Prim p) >>= eval

-- | Default implementation for evaluating lambdas. As a term with a lambda
-- at the head is already in WHNF, this simply returns the term under the
-- lambda with the current local environment.
--
evaluateLam :: Id -> Term -> Eval Value
evaluateLam i x = VLam i x <$> getLocalEnv

-- | Default implementation for evaluating type lambdas. As a term with a type
-- lambda at the head is already in WHNF, this simply returns the term under
-- the type lambda with the current local environment.
--
evaluateTyLam :: TyVar -> Term -> Eval Value
evaluateTyLam i x = VTyLam i x <$> getLocalEnv

-- | Default implementation for evalating a term / type application. This
-- checks if the arguments are applied to a data constructor or primitive, and
-- creates a value if it is fully applied. If the arguments are applied to a
-- function, the first argument is applied.
--
evaluateAppWith
  :: (Term -> Eval Value)
  -> (PrimInfo -> [Either Term Type] -> Eval Value)
  -> (Value -> Either TermOrValue Type -> Eval Value)
  -> Term
  -> Either Term Type
  -> Eval Value
evaluateAppWith eval evalPrim apply x y
  | Data dc <- f
  = if isFullyApplied (dcType dc) args
      then VData dc args <$> getLocalEnv
      else etaExpand term >>= eval

  | Prim p <- f
  , nArgs  <- length . fst $ splitFunForallTy (primType p)
  = case compare (length args) nArgs of
      LT -> etaExpand term >>= eval
      EQ -> evalPrim p args
      GT -> do
        let (pArgs, rArgs) = splitAt nArgs args
        primRes <- evalPrim p pArgs
        foldM apply primRes (first Left <$> rArgs)

  -- Evaluating a function application may change the amount of fuel (e.g. if
  -- the LHS of the application is a recursive function.) If we do not add fuel
  -- back after calling apply, other subterms may not be unfolded as much as
  -- they should be.
  --
  -- This is not needed for data and prim, as they do not evaluate their
  -- arguments, so the fuel will not change.
  --
  | otherwise
  = preserveFuel $ do
      evalF <- eval f
      foldM apply evalF (first Left <$> args)
  where
  term      = either (App x) (TyApp x) y
  (f, args) = collectArgs term

-- | Default implementation for evaluating a letrec expression. This adds all
-- bindings to the heap without eagerly evaluating them, then evaluates the
-- body of the expression.
--
-- Bindings are not evaluated eagerly, as they may not be needed depending how
-- the body of the expression evaluates (i.e. bindings may only be used in a
-- pruned alternative of a case expression).
--
evaluateLetrecWith
  :: (Term -> Eval Value)
  -> [LetBinding]
  -> Term
  -> Eval Value
evaluateLetrecWith eval bs x =
  let terms = fmap (fmap Left) bs
   in withLocals terms (eval x)

-- | Default implementation for evaluating a case expression. This replaces the
-- entire expression with the chosen alternative if it can be statically
-- determined, evaluating that branch only. If there is ambiguity over which
-- alternative to choose, all alternatives are evaluated.
--
-- Predicates are given for determining if a literal or data constructor is
-- matched by a pattern. This is needed as in some frontends (e.g. GHC), there
-- may be types that can be matched with literals or constructors. For example
--
--   case (x :: Integer) of
--     1 -> ...
--
-- and
--
--   case (x :: Integer) of
--     S# 1 -> ...
--
-- match the same value, but do so in different ways. This means the function
-- to check if a literal is matched would have to check both literal patterns
-- and data patterns for the constructors of Integer.
--
-- TODO: Handle undefined scrutinees in case expressions. These should result
-- in the entire expression being replaced with undefined.
--
evaluateCaseWith
  :: (Term -> Eval Value)
  -> (Literal -> Pat -> Eval PatResult)
  -> (DataCon -> [Either Term Type] -> Pat -> Eval PatResult)
  -> Term
  -> Type
  -> [Alt]
  -> Eval Value
evaluateCaseWith eval matchLit matchData x ty alts
  | [(DefaultPat, y)] <- alts
  = eval y

  | otherwise
  = do
      -- TODO: Restore ticks for scrutinee?
      evalX <- fst . collectValueTicks <$> eval x

      case evalX of
        VLit l -> evalMatchingAlt (matchLit l) alts
        VData dc args _env -> evalMatchingAlt (matchData dc args) alts

        v -> do
          evalAlts <- traverse (traverse eval) alts
          pure (VNeu (NeCase v ty evalAlts))
 where
  evalMatchingAlt p =
    -- If the scrutinee is a non-neutral value, and there are no matching
    -- patterns, the case expression must not cover all patterns.
    go (error "findMatchingAlt: No matching pattern in case expression")
   where
    go best []     = eval best
    go best (a:as) =
      p (fst a) >>= \case
        NoMatch -> go best as

        Match tys ids
          | fst a == DefaultPat -> go (snd a) as
          | otherwise -> withTypes tys $ withLocals (fmap Left <$> ids) (eval (snd a))

-- | Default implementation for evaluating a cast expression. This simply
-- evalautes the expression under the cast, keeping the original cast in place.
--
evaluateCastWith
  :: (Term -> Eval Value)
  -> Term
  -> Type
  -> Type
  -> Eval Value
evaluateCastWith eval x a b = do
  evalX <- eval x
  pure (VCast evalX a b)

-- | Default implementation for evaluating a ticked expression. This simply
-- evaluates the expression, keeping the original ticks.
--
evaluateTickWith
  :: (Term -> Eval Value)
  -> TickInfo
  -> Term
  -> Eval Value
evaluateTickWith eval t x = do
  evalX <- eval x
  pure (VTick evalX t)

-- Apply a value or type to a value. This is effectively the beta-reduction
-- rule for evaluation. There are three to consider:
--
--   * application to a stuck term, where a new stuck term is created with
--     the argument applied
--
--   * application to a lambda / type lambda, where the body of the lambda
--     is evaluated in an environment extended with the newly applied binder
--
--   * an illegal application, which means the evaluator was given an
--     ill-formed term
--
-- See the note "environment machines" in Clash.Core.Evaluator.Models for more
-- information about this style of reduction.
--
applyWith
  :: (Term -> Eval Value)
  -> Value
  -> Either TermOrValue Type
  -> Eval Value
applyWith eval val arg
  -- Apply an argument to a stuck term, yielding a new stuck term which
  -- represents an application / type application.
  | VNeu n <- f
  = case arg of
      -- The applied argument is unevaluated, so create a thunk and stop.
      Left (Left t) ->
        VNeu . NeApp n . VThunk t <$> getLocalEnv

      -- The applied argument has been evalauted, so apply the WHNF value
      -- directly to the neutral value.
      Left (Right v) ->
        pure $ VNeu (NeApp n v)

      -- The applied argument is a type, and doesn't need evaluation.
      Right ty ->
        pure $ VNeu (NeTyApp n ty)

  -- Add the new value to the environment and continue. This is analagous to
  -- lazily performing the substitution (actual substitution occurs in
  -- 'evaluateVarWith' when evaluating subterms).
  | VLam i x env <- f
  , Left argV <- arg
  = withLocalEnv env $ withLocal (i, argV) (eval x)

  -- Add the new type to the environment and continue. This is to prevent
  -- losing type information when evaluating to WHNF, e.g.
  --
  --   TyApp (Lam i x) a
  --
  -- would lose knowledge of the applied type, a, when evaluting to WHNF. To
  -- prevent this, types are kept and substituted in the AsTerm instance for
  -- Value (see Clash.Core.Evaluator.Models for this).
  --
  | VTyLam i x env <- f
  , Right argTy <- arg
  = withLocalEnv env $ withType (i, argTy) (eval x)

  -- The term is neither stuck, nor a lambda. That means it does not support
  -- beta reduction, and something has gone horribly wrong.
  | otherwise
  = error ("applyWith: Cannot apply " <> show arg <> " to " <> show val)
 where
  -- TODO Where to add ticks back?
  (f, _) = collectValueTicks val

-- | Recursively evaluate and eta-expand a WHNF value, using the given
-- evaluation function. This converts WHNF to beta-normal eta-long form (NF).
--
quoteWith
  :: (Term -> Eval Value)
  -> Value
  -> Eval Nf
quoteWith eval = go
 where
  apply = applyWith eval

  go = \case
    VNeu n -> fmap NNeu (goNe n)
    VLit l -> pure (NLit l)
    VData dc args env -> quoteDataWith (eval >=> go) dc args env
    VPrim p args env -> quotePrimWith (eval >=> go) p args env
    VLam i x env -> quoteLamWith go apply (Left i) x env
    VTyLam i x env -> quoteLamWith go apply (Right i) x env
    VCast x a b -> quoteCastWith go x a b
    VTick x t -> quoteTickWith go x t
    VThunk x env -> quoteThunkWith (eval >=> go) x env

  goNe = \case
    NeVar v -> quoteNeVar v
    NePrim p args -> quoteNePrimWith go p args
    NeApp x y -> quoteNeAppWith go goNe x y
    NeTyApp x ty -> quoteNeTyAppWith goNe x ty
    NeCase x ty xs -> quoteNeCaseWith go x ty xs

-- These functions are internal details. There is only one sensible way to
-- implement quote, so only quoteWith needs to be exported.

quoteDataWith
  :: (Term -> Eval Nf)
  -> DataCon
  -> [Either Term Type]
  -> LocalEnv
  -> Eval Nf
quoteDataWith quote dc args env =
  withLocalEnv env $ do
    quoteArgs <- traverse (bitraverse quote pure) args
    pure (NData dc quoteArgs)

quotePrimWith
  :: (Term -> Eval Nf)
  -> PrimInfo
  -> [Either Term Type]
  -> LocalEnv
  -> Eval Nf
quotePrimWith quote p args env =
  withLocalEnv env $ do
    quoteArgs <- traverse (bitraverse quote pure) args
    pure (NPrim p quoteArgs)

quoteLamWith
  :: (Value -> Eval Nf)
  -> (Value -> Either TermOrValue Type -> Eval Value)
  -> Either Id TyVar
  -> Term
  -> LocalEnv
  -> Eval Nf
quoteLamWith quote apply i x env =
  case i of
    Left iId ->
      withLocalEnv env $ do
        evalX  <- apply (VLam iId x env) (Left . Right $ VNeu (NeVar iId))
        quoteX <- quote evalX
        pure (NLam iId quoteX)

    Right iTv ->
      withLocalEnv env $ do
        evalX  <- apply (VTyLam iTv x env) (Right (VarTy iTv))
        quoteX <- quote evalX
        pure (NTyLam iTv quoteX)

quoteCastWith
  :: (Value -> Eval Nf)
  -> Value
  -> Type
  -> Type
  -> Eval Nf
quoteCastWith quote x a b = do
  quoteX <- quote x
  pure (NCast quoteX a b)

quoteTickWith
  :: (Value -> Eval Nf)
  -> Value
  -> TickInfo
  -> Eval Nf
quoteTickWith quote x t = do
  quoteX <- quote x
  pure (NTick quoteX t)

quoteThunkWith
  :: (Term -> Eval Nf)
  -> Term
  -> LocalEnv
  -> Eval Nf
quoteThunkWith eval x env =
  withLocalEnv env (eval x)

quoteNeVar :: Id -> Eval (Neutral Nf)
quoteNeVar = pure . NeVar

quoteNePrimWith
  :: (Value -> Eval Nf)
  -> PrimInfo
  -> [Either Value Type]
  -> Eval (Neutral Nf)
quoteNePrimWith quote p args = do
  quoteArgs <- traverse (bitraverse quote pure) args
  pure (NePrim p quoteArgs)

quoteNeAppWith
  :: (Value -> Eval Nf)
  -> (Neutral Value -> Eval (Neutral Nf))
  -> Neutral Value
  -> Value
  -> Eval (Neutral Nf)
quoteNeAppWith quote quoteNe x y = do
  quoteX <- quoteNe x
  quoteY <- quote y
  pure (NeApp quoteX quoteY)

quoteNeTyAppWith
  :: (Neutral Value -> Eval (Neutral Nf))
  -> Neutral Value
  -> Type
  -> Eval (Neutral Nf)
quoteNeTyAppWith quote x ty = do
  quoteX <- quote x
  pure (NeTyApp quoteX ty)

quoteNeCaseWith
  :: (Value -> Eval Nf)
  -> Value
  -> Type
  -> [(Pat, Value)]
  -> Eval (Neutral Nf)
quoteNeCaseWith quote x ty xs = do
  quoteX  <- quote x
  quoteXs <- traverse (bitraverse pure quote) xs
  pure (NeCase quoteX ty quoteXs)

