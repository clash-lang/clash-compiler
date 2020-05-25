{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PartialEval
  ( ghcEvaluator
  ) where

import Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State
import GHC.Integer.GMP.Internals (Integer(..))

import Clash.Core.DataCon
import Clash.Core.Evaluator.Models
import Clash.Core.Evaluator.Semantics
import Clash.Core.Literal
import Clash.Core.Term
import Clash.Core.Type

ghcEvaluator :: Evaluator
ghcEvaluator = Evaluator ghcEvaluate ghcQuote

ghcEvaluate :: Term -> State Env Value
ghcEvaluate term =
  case term of
    Var v -> evaluateVarWith ghcEvaluate v
    Literal l -> evaluateLiteral l
    Data dc -> evaluateDataWith ghcEvaluate dc
    Prim p -> evaluatePrimWith ghcEvaluate ghcEvaluatePrim p
    Lam x e -> evaluateLam x e
    TyLam x e -> evaluateTyLam x e
    App x y -> evaluateAppWith ghcEvaluate ghcEvaluatePrim ghcApply x (Left y)
    TyApp x ty -> evaluateAppWith ghcEvaluate ghcEvaluatePrim ghcApply x (Right ty)
    Letrec bs x -> evaluateLetrecWith ghcEvaluate bs x
    Case x ty xs -> evaluateCaseWith ghcEvaluate ghcIsLiteral ghcIsData x ty xs
    Cast x a b -> evaluateCastWith ghcEvaluate x a b
    Tick ti x -> evaluateTickWith ghcEvaluate ti x

-- TODO Implement evaluation for primitives and call here.
--
ghcEvaluatePrim :: PrimInfo -> [Either Value Type] -> State Env Value
ghcEvaluatePrim p args = pure (VNeu $ NePrim p args)

ghcIsLiteral :: Literal -> Pat -> Bool
ghcIsLiteral l = \case
  DataPat c [] [_]
    |  IntegerLiteral i <- l
    -> case i of
         S# _  -> dcTag c == 1
         Jp# _ -> dcTag c == 2
         Jn# _ -> dcTag c == 3

    |  NaturalLiteral i <- l
    -> case i of
         S# _  -> dcTag c == 1 && i >= 0
         Jp# _ -> dcTag c == 2
         Jn# _ -> False

    |  otherwise
    -> False

  LitPat m -> l == m
  DefaultPat -> True
  _ -> False

ghcIsData :: DataCon -> Pat -> Bool
ghcIsData dc = \case
  DataPat c _ _ -> dc == c
  LitPat _ -> False
  DefaultPat -> True

ghcApply :: Value -> Either Value Type -> State Env Value
ghcApply val arg
  | VNeu n <- f
  = pure $ VNeu (either (NeApp n) (NeTyApp n) arg)

  | VLam i x env <- f
  , Left argV <- arg
  , addBinder <- insertLocal i (Right argV)
  = State.put env >> State.withState addBinder (ghcEvaluate x)

  | VTyLam i x env <- f
  , Right argTy <- arg
  , addBinder <- insertType i argTy
  = State.put env >> State.withState addBinder (ghcEvaluate x)

  | otherwise
  = error ("ghcApply: Cannot apply " <> show arg <> " to " <> show val)
 where
  -- TODO Where to add ticks back?
  (f, _) = collectValueTicks val

ghcQuote :: Value -> State Env Nf
ghcQuote = \case
  VNeu n -> fmap NNeu (ghcQuoteNeutral n)
  VLit l -> quoteLiteral l
  VData dc args -> quoteDataWith ghcQuote dc args
  VPrim p args -> quotePrimWith ghcQuote p args
  VLam i x env -> quoteLamWith ghcQuote ghcApply (Left i) x env
  VTyLam i x env -> quoteLamWith ghcQuote ghcApply (Right i) x env
  VCast x a b -> quoteCastWith ghcQuote x a b
  VTick x t -> quoteTickWith ghcQuote x t

ghcQuoteNeutral :: Neutral Value -> State Env (Neutral Nf)
ghcQuoteNeutral = \case
  NeVar v -> quoteNeVar v
  NePrim p args -> quoteNePrimWith ghcQuote p args
  NeApp x y -> quoteNeAppWith ghcQuote ghcQuoteNeutral x y
  NeTyApp x ty -> quoteNeTyAppWith ghcQuoteNeutral x ty
  NeCase x ty xs -> quoteNeCaseWith ghcQuote x ty xs

