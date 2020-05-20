{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PartialEval
  ( ghcEvaluator
  ) where

import Control.Monad ((>=>), foldM)
import Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State
import Data.Bifunctor (bimap)
import Data.Bitraversable (bitraverse)
import GHC.Integer.GMP.Internals (Integer(..))

import Clash.Core.DataCon
import Clash.Core.Evaluator.Models
import Clash.Core.Evaluator.Semantics
import Clash.Core.Literal
import Clash.Core.Term
import Clash.Core.TermInfo (applyTypeToArgs)
import Clash.Core.TyCon (TyConMap)
import Clash.Core.Type
import Clash.Core.Var

ghcEvaluator :: Evaluator
ghcEvaluator = Evaluator ghcEvaluate ghcQuote

ghcEvaluate :: Term -> State Env Value
ghcEvaluate term =
  case term of
    Var v -> evaluateVarWith ghcEvaluate v
    Literal l -> evaluateLiteral l
    Data dc
      | isFullyApplied (dcType dc) [] -> pure (VData dc [])
      | otherwise -> expandAndEval term

    Prim p
      | isFullyApplied (primType p) [] -> ghcEvaluatePrim p []
      | otherwise -> expandAndEval term

    Lam x e -> evaluateLam x e
    TyLam x e -> evaluateTyLam x e
    App x y
      | Data dc <- f ->
          if isFullyApplied (dcType dc) as
            then VData dc <$> evalArgs as
            else expandAndEval term

      | Prim p <- f ->
          let argN = length . fst $ splitFunForallTy (primType p)
           in case compare (length as) argN of
                LT -> expandAndEval term
                EQ -> evalArgs as >>= ghcEvaluatePrim p
                GT -> do
                  (pArgs, rArgs) <- splitAt argN <$> evalArgs as
                  primRes <- ghcEvaluatePrim p pArgs
                  ghcApplyAll primRes rArgs

      | otherwise -> evaluateAppWith ghcEvaluate ghcApply x y

    TyApp x ty
      | Data dc <- f ->
          if isFullyApplied (dcType dc) as
            then VData dc <$> evalArgs as
            else expandAndEval term

      | Prim p <- f ->
          let argN = length . fst $ splitFunForallTy (primType p)
           in case compare (length as) argN of
                LT -> expandAndEval term
                EQ -> evalArgs as >>= ghcEvaluatePrim p
                GT -> do
                  (pArgs, rArgs) <- splitAt argN <$> evalArgs as
                  primRes <- ghcEvaluatePrim p pArgs
                  ghcApplyAll primRes rArgs

      | otherwise -> evaluateTyAppWith ghcEvaluate ghcTyApply x ty

    Letrec bs x -> evaluateLetrecWith ghcEvaluate bs x
    Case x ty xs -> evaluateCaseWith ghcEvaluate ghcIsLiteral ghcIsData x ty xs
    Cast x a b -> evaluateCastWith ghcEvaluate x a b
    Tick ti x -> evaluateTickWith ghcEvaluate ti x
 where
  (f, as)  = collectArgs term
  evalArgs = traverse (bitraverse ghcEvaluate pure)

  expandAndEval =
    etaExpand >=> ghcEvaluate

  isFullyApplied ty args =
    length args == length (fst $ splitFunForallTy ty)

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

ghcApplyAll :: Value -> [Either Value Type] -> State Env Value
ghcApplyAll val = foldM applyArg val
 where
  applyArg x arg = either (ghcApply x) (ghcTyApply x) arg

ghcApply :: Value -> Value -> State Env Value
ghcApply x y = do
  res <- case x' of
    VNeu n -> pure (VNeu (NeApp n y))
    VLam i x'' env ->
      let addBinder = insertLocal i (Right y)
       in State.put env >> State.withState addBinder (ghcEvaluate x'')

    _ -> error ("ghcApply: Cannot apply value to " <> show x')

  pure (addTicks res ts)
 where
  (x', ts) = collectValueTicks x

ghcTyApply :: Value -> Type -> State Env Value
ghcTyApply x ty = do
  res <- case x' of
    VNeu n -> pure (VNeu (NeTyApp n ty))
    VTyLam i x'' env ->
      let addBinder = insertType i ty
       in State.put env >> State.withState addBinder (ghcEvaluate x'')

    _ -> error ("ghcTyApply: Cannot apply type to " <> show x')

  pure (addTicks res ts)
 where
  (x', ts) = collectValueTicks x

ghcQuote :: Value -> State Env Nf
ghcQuote = \case
  VNeu n -> fmap NNeu (ghcQuoteNeutral n)
  VLit l -> quoteLiteral l
  VData dc args -> quoteDataWith ghcQuote dc args
  VPrim p args -> quotePrimWith ghcQuote p args
  VLam i x env -> quoteLamWith ghcQuote ghcApply i x env
  VTyLam i x env -> quoteTyLamWith ghcQuote ghcTyApply i x env
  VCast x a b -> quoteCastWith ghcQuote x a b
  VTick x t -> quoteTickWith ghcQuote x t

ghcQuoteNeutral :: Neutral Value -> State Env (Neutral Nf)
ghcQuoteNeutral = \case
  NeVar v -> quoteNeVar v
  NePrim p args -> quoteNePrimWith ghcQuote p args
  NeApp x y -> quoteNeAppWith ghcQuote ghcQuoteNeutral x y
  NeTyApp x ty -> quoteNeTyAppWith ghcQuoteNeutral x ty
  NeCase x ty xs -> quoteNeCaseWith ghcQuote x ty xs

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

