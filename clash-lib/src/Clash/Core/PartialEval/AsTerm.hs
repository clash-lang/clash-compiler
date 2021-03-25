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
import qualified Data.Map.Strict as Map

import Clash.Core.HasFreeVars
import Clash.Core.PartialEval.NormalForm
import Clash.Core.Subst
import Clash.Core.Term (Term(..), Pat, Alt, mkApps)
import Clash.Core.Var (Var(varUniq))
import Clash.Core.VarEnv

-- | Convert a term in some normal form back into a Term.
--
class AsTerm a where
  -- | Convert a term in some normal form back into a Term. If the normal form
  -- keeps track of an environment (e.g. Value) this function will also let
  -- bind anything from the environment that would otherwise appear free.
  asTerm :: a -> Term

  -- | Convert a term in some normal form back into a Term. If the normal form
  -- keeps track of an environment (e.g. Value) this function will not let bind
  -- anything from the environment, meaning they may appear free in the result.
  --
  -- This is used to produce terms to use with functions like isWorkFree, where
  -- using asTerm would result in a work-free term being identified as doing
  -- work because it relies on some let bound definition which performs work.
  unsafeAsTerm :: a -> Term

instance (AsTerm a) => AsTerm (Neutral a) where
  asTerm = unsafeAsTerm

  unsafeAsTerm = \case
    NeVar i -> Var i
    NePrim pr args -> mkApps (Prim pr) (unsafeArgsToTerms args)
    NeApp x y -> App (unsafeAsTerm x) (unsafeAsTerm y)
    NeTyApp x ty -> TyApp (unsafeAsTerm x) ty
    NeLetrec bs x ->
      let bs' = fmap (second unsafeAsTerm) bs
          x'  = unsafeAsTerm x
       in Letrec bs' x'

    NeCase x ty alts -> Case (unsafeAsTerm x) ty (unsafeAltsToTerms alts)

instance AsTerm Value where
  asTerm value =
    case value of
      VData _ _ env ->
        substEnv (lenvValues env) (unsafeAsTerm value)
        -- bindEnv (unsafeAsTerm value) env

      VLam _ _ env ->
        substEnv (lenvValues env) (unsafeAsTerm value)
        -- bindEnv (unsafeAsTerm value) env

      VTyLam _ _ env ->
        substEnv (lenvValues env) (unsafeAsTerm value)
        -- bindEnv (unsafeAsTerm value) env

      VCast x a b ->
        Cast (asTerm x) a b

      VTick x t ->
        Tick t (asTerm x)

      VThunk _ env ->
        substEnv (lenvValues env) (unsafeAsTerm value)
        -- bindEnv (unsafeAsTerm value) env

      _ ->
        unsafeAsTerm value
   where
    substEnv binds term =
      let fvs0 = freeVarsOf term
          (used, rest) = Map.partitionWithKey (\k _ -> elemVarSet k fvs0) binds
          (fvs1, vals) = substValues used fvs0
       in if null vals then term else
            let tms  = fmap (second asTerm) vals
                fvs2 = unionVarSet fvs1 (freeVarsOf (fmap snd tms))
                iss  = mkInScopeSet fvs2
                sub  = extendIdSubstList (mkSubst iss) tms
             in substEnv rest (substTm "substEnv" sub term)

    substValues binds fvs =
      let f i x acc@(vars, vals) =
            case i `elemVarSet` vars of
              True  -> (delVarSetByKey (varUniq i) vars, (i, x) : vals)
              False -> acc

       in Map.foldrWithKey f (fvs, mempty) binds

{-
    bindEnv :: Term -> LocalEnv -> Term
    bindEnv x env =
      let fvs = localFVsOfTerms [x]
          binds = go [] fvs (lenvValues env)
       in if null binds then x else Letrec (go [] fvs (lenvValues env)) x

    go :: [LetBinding] -> VarSet -> Map Id Value -> [LetBinding]
    go acc fvs bindings
      | nullVarSet fvs = acc
      | otherwise =
          let (used, rest) = Map.partitionWithKey (\k _ -> k `elemVarSet` fvs) bindings
              used' = Map.toList (fmap unsafeAsTerm used)
              fvs' = localFVsOfTerms (fmap snd used')
           in go (acc <> used') fvs' rest
-}

  unsafeAsTerm = \case
    VNeutral neu -> unsafeAsTerm neu
    VLiteral lit -> Literal lit
    VData dc args _ -> mkApps (Data dc) (unsafeArgsToTerms args)
    VLam i x _ -> Lam i x
    VTyLam i x _ -> TyLam i x
    VCast x a b -> Cast (unsafeAsTerm x) a b
    VTick x tick -> Tick tick (unsafeAsTerm x)
    VThunk x _ -> x

instance AsTerm Normal where
  asTerm = unsafeAsTerm

  unsafeAsTerm = \case
    NNeutral neu -> unsafeAsTerm neu
    NLiteral lit -> Literal lit
    NData dc args -> mkApps (Data dc) (unsafeArgsToTerms args)
    NLam i x -> Lam i (unsafeAsTerm x)
    NTyLam i x -> TyLam i (unsafeAsTerm x)
    NCast x a b -> Cast (unsafeAsTerm x) a b
    NTick x tick -> Tick tick (unsafeAsTerm x)

unsafeArgsToTerms :: (AsTerm a) => Args a -> Args Term
unsafeArgsToTerms = fmap $ first unsafeAsTerm

unsafeAltsToTerms :: (AsTerm a) => [(Pat, a)] -> [Alt]
unsafeAltsToTerms = fmap $ second unsafeAsTerm
