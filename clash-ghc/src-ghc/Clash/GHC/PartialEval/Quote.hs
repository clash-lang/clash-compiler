{-|
Copyright   : (C) 2020, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

This module provides the "quoting" part of the partial evaluator, which
traverses a WHNF value, recursively evaluating sub-terms to remove redexes.
-}

{-# LANGUAGE LambdaCase #-}

module Clash.GHC.PartialEval.Quote
  ( quote
  ) where

import Data.Bitraversable

import Clash.Core.DataCon (DataCon)
import Clash.Core.PartialEval.Monad
import Clash.Core.PartialEval.NormalForm
import Clash.Core.Term (Term, PrimInfo, TickInfo, Pat)
import Clash.Core.Type (Type)
import Clash.Core.Var (Id, TyVar)

import Clash.GHC.PartialEval.Eval

quote :: Value -> Eval Normal
quote = \case
  VNeutral n -> NNeutral <$> quoteNeutral n
  VLiteral l -> pure (NLiteral l)
  VData dc args env -> quoteData dc args env
  VLam i x env -> quoteLam i x env
  VTyLam i x env -> quoteTyLam i x env
  VCast x a b -> quoteCast x a b
  VTick x tick -> quoteTick x tick
  VThunk x env -> quoteThunk x env

quoteNeutral :: Neutral Value -> Eval (Neutral Normal)
quoteNeutral = \case
  NeVar i -> pure (NeVar i)
  NePrim pr args -> quoteNePrim pr args
  NeApp x y -> quoteNeApp x y
  NeTyApp x ty -> quoteNeTyApp x ty
  NeLetrec bs x -> quoteNeLetrec bs x
  NeCase x ty alts -> quoteNeCase x ty alts

quoteArgs :: Args Value -> Eval (Args Normal)
quoteArgs = traverse (bitraverse quote normTy)

quoteAlts :: [(Pat, Value)] -> Eval [(Pat, Normal)]
quoteAlts = traverse (bitraverse pure quote)

quoteBinders :: [(Id, Value)] -> Eval [(Id, Normal)]
quoteBinders = traverse (bitraverse pure quote)

quoteData :: DataCon -> Args Value -> LocalEnv -> Eval Normal
quoteData dc args env = setLocalEnv env (NData dc <$> quoteArgs args)

quoteLam :: Id -> Term -> LocalEnv -> Eval Normal
quoteLam i x env =
  setLocalEnv env $
    fmap (NLam i) (eval x >>= quote)

quoteTyLam :: TyVar -> Term -> LocalEnv -> Eval Normal
quoteTyLam i x env =
  setLocalEnv env $
    fmap (NTyLam i) (eval x >>= quote)

quoteCast :: Value -> Type -> Type -> Eval Normal
quoteCast x a b = NCast <$> quote x <*> pure a <*> pure b

quoteTick :: Value -> TickInfo -> Eval Normal
quoteTick x tick = NTick <$> quote x <*> pure tick

quoteThunk :: Term -> LocalEnv -> Eval Normal
quoteThunk x env = setLocalEnv env (eval x >>= quote)

quoteNePrim :: PrimInfo -> Args Value -> Eval (Neutral Normal)
quoteNePrim pr = fmap (NePrim pr) . quoteArgs

quoteNeApp :: Neutral Value -> Value -> Eval (Neutral Normal)
quoteNeApp x y = NeApp <$> quoteNeutral x <*> quote y

quoteNeTyApp :: Neutral Value -> Type -> Eval (Neutral Normal)
quoteNeTyApp x ty = NeTyApp <$> quoteNeutral x <*> pure ty

quoteNeLetrec :: [(Id, Value)] -> Value -> Eval (Neutral Normal)
quoteNeLetrec bs x =
  withIds bs (NeLetrec <$> quoteBinders bs <*> quote x)

quoteNeCase :: Value -> Type -> [(Pat, Value)] -> Eval (Neutral Normal)
quoteNeCase x ty alts =
  NeCase <$> quote x <*> pure ty <*> quoteAlts alts
