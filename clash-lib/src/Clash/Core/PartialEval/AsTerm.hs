{-|
Copyright   : (C) 2020-2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

The AsTerm class and relevant instances for the partial evaluator. This
defines how to convert normal forms back into Terms which can be given as the
result of evaluation.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Core.PartialEval.AsTerm
  ( AsTerm(..)
  ) where

import Data.Bifunctor (first, second)

import Clash.Core.HasFreeVars
import Clash.Core.PartialEval.NormalForm
import Clash.Core.Term (Bind(..), Term(..), Pat, Alt, mkApps)
import Clash.Core.VarEnv (elemVarSet)

-- | Convert a term in some normal form back into a Term. This is important,
-- as it may perform substitutions which have not yet been performed (i.e. when
-- converting from WHNF where heads contain the environment at that point).
--
class AsTerm a where
  asTerm:: a -> Term

instance (AsTerm a) => AsTerm (Neutral a) where
  asTerm = \case
    NeVar i -> Var i
    NePrim pr args -> mkApps (Prim pr) (argsToTerms args)
    NeApp x y -> App (asTerm x) (asTerm y)
    NeTyApp x ty -> TyApp (asTerm x) ty
    NeLet bs x -> removeUnusedBindings (fmap asTerm bs) (asTerm x)
    NeCase x ty alts -> Case (asTerm x) ty (altsToTerms alts)

removeUnusedBindings :: Bind Term -> Term -> Term
removeUnusedBindings bs x
  | isUsed bs = Let bs x
  | otherwise = x
 where
  free = freeVarsOf x

  isUsed = \case
    NonRec i _ -> elemVarSet i free
    Rec xs -> any (flip elemVarSet free . fst) xs

instance AsTerm Value where
  asTerm = \case
    VNeutral neu -> asTerm neu
    VLiteral lit -> Literal lit
    VData dc args _env -> mkApps (Data dc) (argsToTerms args)
    VLam i x _env -> Lam i x
    VTyLam i x _env -> TyLam i x
    VCast x a b -> Cast (asTerm x) a b
    VTick x tick -> Tick tick (asTerm x)
    VThunk x _env -> x

instance AsTerm Normal where
  asTerm = \case
    NNeutral neu -> asTerm neu
    NLiteral lit -> Literal lit
    NData dc args -> mkApps (Data dc) (argsToTerms args)
    NLam i x _env -> Lam i (asTerm x)
    NTyLam i x _env -> TyLam i (asTerm x)
    NCast x a b -> Cast (asTerm x) a b
    NTick x tick -> Tick tick (asTerm x)

argsToTerms :: (AsTerm a) => Args a -> Args Term
argsToTerms = fmap $ first asTerm

altsToTerms :: (AsTerm a) => [(Pat, a)] -> [Alt]
altsToTerms = fmap $ second asTerm
