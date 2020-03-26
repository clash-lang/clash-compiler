{-# LANGUAGE MultiWayIf #-}

module Clash.Core.Evaluator.Semantics
  ( evaluateVarWith
  , evaluateLiteral
  , evaluateData
  , evaluatePrimWith
  , evaluateLam
  , evaluateTyLam
  , evaluateAppWith
  , evaluateTyAppWith
  , evaluateLetrecWith
  , evaluateCaseWith
  , evaluateCastWith
  , evaluateTickWith
  , quoteLiteral
  , quoteDataWith
  , quotePrimWith
  , quoteLamWith
  , quoteTyLamWith
  , quoteCastWith
  , quoteTickWith
  , quoteNeVar
  , quoteNeData
  , quoteNePrim
  , quoteNeAppWith
  , quoteNeTyAppWith
  , quoteNeCaseWith
  ) where

import Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State
import Data.Bitraversable (bitraverse)
import Data.Foldable (foldl')

import BasicTypes (InlineSpec(..))

import Clash.Core.DataCon
import Clash.Core.Evaluator.Models
import Clash.Core.Literal
import Clash.Core.Term
import Clash.Core.Type
import Clash.Core.Var
import Clash.Driver.Types (Binding(..))

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
     -> if canInline (bindingSpec b)
           then either (State.withState (deleteGlobal i) . eval) pure (bindingTerm b)
           else pure (VNeu (NeVar i))

     |  otherwise
     -> error ("evaluateVarWith: Variable not in environment: " <> show i)
 where
  canInline spec = spec == Inline || spec == Inlinable

-- | Default implementation for evaluating a literal.
-- This simply wraps the literal up into a Value.
--
evaluateLiteral :: Literal -> State Env Value
evaluateLiteral = pure . VLit

-- | Default implementation for evaluating a data constructor. This checks the
-- arity of the constructor, and returns VData if it is nullary, or NeData if
-- it takes arguments.
--
evaluateData :: DataCon -> State Env Value
evaluateData dc
  | null argTys = pure (VData dc [])
  | otherwise = pure (VNeu (NeData dc []))
 where
  argTys = fst $ splitFunForallTy (dcType dc)

-- | Default implementation for evaluating a primitive operation. This checks
-- the arity of the operation, and attempts delta reduction if it is nullary,
-- or returns NePrim if it takes arguments.
--
evaluatePrimWith
  :: (PrimInfo -> [Either Value Type] -> State Env Value)
  -> PrimInfo
  -> State Env Value
evaluatePrimWith eval p
  | null argTys = eval p []
  | otherwise = pure (VNeu (NePrim p []))
 where
  argTys = fst $ splitFunForallTy (primType p)

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

-- | Default implementation for evalating an application. This evalautes both
-- the function and it's applied argument, applying them using the supplied
-- apply function.
--
evaluateAppWith
  :: (Term -> State Env Value)
  -> (Value -> Value -> State Env Value)
  -> Term
  -> Term
  -> State Env Value
evaluateAppWith eval apply x y = do
  evalX <- eval x
  evalY <- eval y
  apply evalX evalY

-- | Default implementation for evaluating a type application. This evaluates
-- the function, and applies a type to it using the supplied apply function.
--
evaluateTyAppWith
  :: (Term -> State Env Value)
  -> (Value -> Type -> State Env Value)
  -> Term
  -> Type
  -> State Env Value
evaluateTyAppWith eval apply x ty = do
  evalX <- eval x
  apply evalX ty

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
  -> Term
  -> Type
  -> [Alt]
  -> State Env Value
evaluateCaseWith eval x ty ys = do
  evalX <- eval x

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
  matchLit lit (LitPat l) = lit == l
  matchLit _ DefaultPat = True
  matchLit _ _ = False

  matchData dc (DataPat c _ _) = dc == c
  matchData _ DefaultPat = True
  matchData _ _ = False
    
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
  -> (Value -> Value -> State Env Value)
  -> Id
  -> Term
  -> Env
  -> State Env Nf
quoteLamWith quote apply i x env = do
  evalX  <- apply (VLam i x env) (VNeu (NeVar i))
  quoteX <- quote evalX
  pure (NLam i quoteX)

quoteTyLamWith
  :: (Value -> State Env Nf)
  -> (Value -> Type -> State Env Value)
  -> TyVar
  -> Term
  -> Env
  -> State Env Nf
quoteTyLamWith quote apply i x env = do
  evalX  <- apply (VTyLam i x env) (VarTy i)
  quoteX <- quote evalX
  pure (NTyLam i quoteX)

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

quoteNeData :: DataCon -> [Either Value Type] -> State Env (Neutral Nf)
quoteNeData = undefined

quoteNePrim :: PrimInfo -> [Either Value Type] -> State Env (Neutral Nf)
quoteNePrim = undefined

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

