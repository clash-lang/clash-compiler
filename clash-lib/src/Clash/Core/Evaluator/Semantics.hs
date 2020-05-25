{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Core.Evaluator.Semantics
  ( evaluateVarWith
  , evaluateLiteral
  , evaluateDataWith
  , evaluatePrimWith
  , evaluateLam
  , evaluateTyLam
  , evaluateAppWith
  , evaluateLetrecWith
  , evaluateCaseWith
  , evaluateCastWith
  , evaluateTickWith
  , quoteLiteral
  , quoteDataWith
  , quotePrimWith
  , quoteLamWith
  , quoteCastWith
  , quoteTickWith
  , quoteNeVar
  , quoteNePrimWith
  , quoteNeAppWith
  , quoteNeTyAppWith
  , quoteNeCaseWith
  ) where

import Control.Monad (foldM)
import Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State
import Data.Bifunctor (bimap)
import Data.Bitraversable (bitraverse)
import Data.Foldable (foldl')

import BasicTypes (InlineSpec(..))

import Clash.Core.DataCon
import Clash.Core.Evaluator.Models
import Clash.Core.Literal
import Clash.Core.Term
import Clash.Core.TermInfo
import Clash.Core.TyCon
import Clash.Core.Type
import Clash.Core.Var
import Clash.Driver.Types (Binding(..))

isFullyApplied :: Type -> [Either Term Type] -> Bool
isFullyApplied ty args =
  length args == length (fst $ splitFunForallTy ty)

etaExpand :: Term -> State Env Term
etaExpand x =
  case collectArgs x of
    y@(Data dc, _) -> do
      tcm <- State.gets envTcMap
      expand tcm (dcType dc) y

    y@(Prim p, _) -> do
      tcm <- State.gets envTcMap
      expand tcm (primType p) y

    _ -> pure x
 where
  expand :: TyConMap -> Type -> (Term, [Either Term Type]) -> State Env Term
  expand tcm ty (tm, args) = do
    let missingArgTys = fst $ splitFunForallTy (applyTypeToArgs tm tcm ty args)
    missingArgs <- traverse etaNameOf missingArgTys

    pure $ mkAbstraction
      (mkApps x (fmap (bimap Var VarTy) missingArgs))
      missingArgs

  etaNameOf :: Either TyVar Type -> State Env (Either Id TyVar)
  etaNameOf = \case
    Left tv  -> pure (Right tv)
    Right ty -> Left <$> State.state (mkUniqueId "eta" ty)

-- | Default implementation for looking up a variable in the environment.
-- This checks both the local and global environments, and inlines global
-- binders which are not marked as NOINLINE.
--
evaluateVarWith :: (Term -> State Env Value) -> Id -> State Env Value
evaluateVarWith eval i = do
  env <- State.get

  if |  Just etv <- lookupLocal i env
     -> either (State.withState (deleteLocal i) . eval) pure etv

     |  Just b <- lookupGlobal i env
     ,  canInline (bindingSpec b)
     -> either (State.withState (deleteGlobal i) . eval) pure (bindingTerm b)

     |  otherwise
     -> pure (VNeu (NeVar i))
 where
  canInline spec = spec == Inline || spec == Inlinable

-- | Default implementation for evaluating a literal.
-- This simply wraps the literal up into a Value.
--
evaluateLiteral :: Literal -> State Env Value
evaluateLiteral = pure . VLit

-- | Default implementaion for evaluating data constructors. If the constructor
-- is not fully applied, then it is eta-expanded and its eta-expanded form is
-- evaluated to obtain a result.
--
evaluateDataWith
  :: (Term -> State Env Value)
  -> DataCon
  -> State Env Value
evaluateDataWith eval dc
  | isFullyApplied (dcType dc) [] = pure (VData dc [])
  | otherwise = etaExpand (Data dc) >>= eval

-- | Default implementation for evaluating primitive operations. If the primop
-- is not fully applied, then it is eta-expanded and its eta-expanded form is
-- delta-reduced to obtain a result.
--
evaluatePrimWith
  :: (Term -> State Env Value)
  -> (PrimInfo -> [Either Value Type] -> State Env Value)
  -> PrimInfo
  -> State Env Value
evaluatePrimWith eval evalPrim p
  | isFullyApplied (primType p) [] = evalPrim p []
  | otherwise = etaExpand (Prim p) >>= eval

-- | Default implementation for evaluating lambdas. As a term with a lambda
-- at the head is already in WHNF, this simply returns the term under the
-- lambda with the current environment.
--
evaluateLam :: Id -> Term -> State Env Value
evaluateLam i x = State.gets (VLam i x)

-- | Default implementation for evaluating type lambdas. As a term with a type
-- lambda at the head is already in WHNF, this simply returns the term under
-- the type lambda with the current environment.
--
evaluateTyLam :: TyVar -> Term -> State Env Value
evaluateTyLam i x = State.gets (VTyLam i x)

-- | Default implementation for evalating a term / type application. This
-- checks if the arguments are applied to a data constructor or primitive, and
-- creates a value if it is fully applied. If the arguments are applied to a
-- function, the first argument is applied.
--
evaluateAppWith
  :: (Term -> State Env Value)
  -> (PrimInfo -> [Either Value Type] -> State Env Value)
  -> (Value -> Either Value Type -> State Env Value)
  -> Term
  -> Either Term Type
  -> State Env Value
evaluateAppWith eval evalPrim apply x y
  | Data dc <- f
  = if isFullyApplied (dcType dc) args
       then VData dc <$> evalArgs args
       else etaExpand term >>= eval

  | Prim p <- f
  , nArgs  <- length . fst $ splitFunForallTy (primType p)
  = case compare (length args) nArgs of
      LT -> etaExpand term >>= eval
      EQ -> evalArgs args >>= evalPrim p
      GT -> do
        (pArgs, rArgs) <- splitAt nArgs <$> evalArgs args
        primRes <- evalPrim p pArgs
        foldM apply primRes rArgs

  | otherwise
  = do
       evalX <- eval x
       evalY <- evalArg y
       apply evalX evalY
 where
  term      = either (App x) (TyApp x) y
  (f, args) = collectArgs term
  evalArg   = bitraverse eval pure
  evalArgs  = traverse evalArg

-- | Default implementation for evaluating a letrec expression. This adds all
-- bindings to the heap without eagerly evaluating them, then evaluates the
-- body of the expression.
--
-- Bindings are not evaluated eagerly, as they may not be needed depending how
-- the body of the expression evaluates (i.e. bindings may be in an elided
-- alternative for a case expression).
--
evaluateLetrecWith
  :: (Term -> State Env Value)
  -> [LetBinding]
  -> Term
  -> State Env Value
evaluateLetrecWith eval bs x =
  State.withState addTerms (eval x)
 where
  addTerms env =
    let terms = fmap (fmap Left) bs
     in foldl' (flip $ uncurry insertLocal) env terms

-- | Default implementation for evaluating a case expression. This elides the
-- case expression if the branch taken can be determined statically, otherwise
-- it evaluates all alternatives.
--
-- Only literals and data constructors can be matched on. While this is enough
-- for simple frontends, more complex frontends may want more complex matching
-- rules (e.g. matching on VData with literals or matching on primitives).
--
evaluateCaseWith
  :: (Term -> State Env Value)
  -> (Literal -> Pat -> Bool)
  -> (DataCon -> Pat -> Bool)
  -> Term
  -> Type
  -> [Alt]
  -> State Env Value
evaluateCaseWith eval matchLit matchData x ty ys = do
  evalX <- eval x

  -- TODO Strip ticks?
  case evalX of
    VLit l ->
      case findMatchingAlt (matchLit l) ys of
        Just (_, y) -> eval y
        Nothing -> error ("evaluateCaseWith: No pattern for literal " <> show l)

    VData dc _ ->
      case findMatchingAlt (matchData dc) ys of
        Just (_, y) -> eval y
        Nothing -> error ("evaluateCaseWith: No pattern for data " <> show dc)

    v -> do
      evalYs <- traverse (traverse eval) ys
      pure (VNeu (NeCase v ty evalYs))
 where
  findMatchingAlt p as =
    case filter (p . fst) as of
      [] -> Nothing
      (b:bs)
        | fst b == DefaultPat && not (null bs) -> Just (head bs)
        | otherwise -> Just b

-- | Default implementation for evaluating a cast expression. This simply
-- evalautes the expression under the cast, keeping the original cast in place.
--
evaluateCastWith
  :: (Term -> State Env Value)
  -> Term
  -> Type
  -> Type
  -> State Env Value
evaluateCastWith eval x a b = do
  evalX <- eval x
  pure (VCast evalX a b)

-- | Default implementation for evaluating a ticked expression. This simply
-- evaluates the expression, keeping the original ticks.
--
evaluateTickWith
  :: (Term -> State Env Value)
  -> TickInfo
  -> Term
  -> State Env Value
evaluateTickWith eval t x = do
  evalX <- eval x
  pure (VTick evalX t)

quoteLiteral :: Literal -> State Env Nf
quoteLiteral = pure . NLit

quoteDataWith
  :: (Value -> State Env Nf)
  -> DataCon
  -> [Either Value Type]
  -> State Env Nf
quoteDataWith quote dc args = do
  quoteArgs <- traverse (bitraverse quote pure) args
  pure (NData dc quoteArgs)

quotePrimWith
  :: (Value -> State Env Nf)
  -> PrimInfo
  -> [Either Value Type]
  -> State Env Nf
quotePrimWith quote p args = do
  quoteArgs <- traverse (bitraverse quote pure) args
  pure (NPrim p quoteArgs)

quoteLamWith
  :: (Value -> State Env Nf)
  -> (Value -> Either Value Type -> State Env Value)
  -> Either Id TyVar
  -> Term
  -> Env
  -> State Env Nf
quoteLamWith quote apply i x env =
  case i of
    Left iId  -> do
      evalX  <- apply (VLam iId x env) (Left $ VNeu (NeVar iId))
      quoteX <- quote evalX
      pure (NLam iId quoteX)

    Right iTv -> do
      evalX  <- apply (VTyLam iTv x env) (Right (VarTy iTv))
      quoteX <- quote evalX
      pure (NTyLam iTv quoteX)

quoteCastWith
  :: (Value -> State Env Nf)
  -> Value
  -> Type
  -> Type
  -> State Env Nf
quoteCastWith quote x a b = do
  quoteX <- quote x
  pure (NCast quoteX a b)

quoteTickWith
  :: (Value -> State Env Nf)
  -> Value
  -> TickInfo
  -> State Env Nf
quoteTickWith quote x t = do
  quoteX <- quote x
  pure (NTick quoteX t)

quoteNeVar :: Id -> State Env (Neutral Nf)
quoteNeVar = pure . NeVar

quoteNePrimWith
  :: (Value -> State Env Nf)
  -> PrimInfo
  -> [Either Value Type]
  -> State Env (Neutral Nf)
quoteNePrimWith quote p args = do
  quoteArgs <- traverse (bitraverse quote pure) args
  pure (NePrim p quoteArgs)

quoteNeAppWith
  :: (Value -> State Env Nf)
  -> (Neutral Value -> State Env (Neutral Nf))
  -> Neutral Value
  -> Value
  -> State Env (Neutral Nf)
quoteNeAppWith quote quoteNe x y = do
  quoteX <- quoteNe x
  quoteY <- quote y
  pure (NeApp quoteX quoteY)

quoteNeTyAppWith
  :: (Neutral Value -> State Env (Neutral Nf))
  -> Neutral Value
  -> Type
  -> State Env (Neutral Nf)
quoteNeTyAppWith quote x ty = do
  quoteX <- quote x
  pure (NeTyApp quoteX ty)

quoteNeCaseWith
  :: (Value -> State Env Nf)
  -> Value
  -> Type
  -> [(Pat, Value)]
  -> State Env (Neutral Nf)
quoteNeCaseWith quote x ty xs = do
  quoteX  <- quote x
  quoteXs <- traverse (bitraverse pure quote) xs
  pure (NeCase quoteX ty quoteXs)

