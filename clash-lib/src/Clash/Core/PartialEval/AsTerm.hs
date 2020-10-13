{-|
Copyright   : (C) 2020 QBayLogic B.V.
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
import Data.Graph (SCC(..), flattenSCCs)

import Clash.Core.FreeVars (localFVsOfTerms)
import Clash.Core.PartialEval.NormalForm
import Clash.Core.Term (Term(..), LetBinding, Pat, Alt, mkApps)
import Clash.Core.Util (sccLetBindings)
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
    NeLetrec bs x ->
      let bs' = fmap (second asTerm) bs
          x'  = asTerm x
       in removeUnusedBindings bs' x'

    NeCase x ty alts -> Case (asTerm x) ty (altsToTerms alts)

removeUnusedBindings :: [LetBinding] -> Term -> Term
removeUnusedBindings bs x
  | null used = x
  | otherwise = Letrec used x
 where
  free = localFVsOfTerms [x]
  used = flattenSCCs $ filter isUsed (sccLetBindings bs)

  isUsed = \case
    AcyclicSCC y -> fst y `elemVarSet` free
    CyclicSCC ys -> any (flip elemVarSet free . fst) ys

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

