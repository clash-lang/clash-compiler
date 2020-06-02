{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}

module Clash.GHC.PartialEval
  ( ghcEvaluator
  ) where

import qualified Control.Monad.State.Strict as State
import Data.Bifunctor
import Data.Either (partitionEithers)
import GHC.Integer.GMP.Internals (BigNat(..), Integer(..))
import Unsafe.Coerce

import Clash.Core.DataCon
import Clash.Core.Evaluator.Models
import Clash.Core.Evaluator.Semantics
import Clash.Core.Literal
import Clash.Core.Term
import Clash.Core.TyCon
import Clash.Core.TysPrim
import Clash.Core.Type
import Clash.Core.Var
import Clash.Unique

-- An evaluator for partial evaluation that uses GHC specific details. This
-- allows the evaluator to be implemented with an understanding of built-in
-- GHC types and primitives which can appear in Clash core when using GHC as a
-- compiler front-end.
--
ghcEvaluator :: Evaluator
ghcEvaluator = evaluatorWith ghcMatchLiteral ghcMatchData ghcEvaluatePrim

-- TODO Implement evaluation for primitives and call here.
--
ghcEvaluatePrim :: PrimInfo -> [Either Value Type] -> Eval Value
ghcEvaluatePrim p args = pure (VNeu $ NePrim p args)

-- | Attempt to match a literal against a pattern. If the pattern matches
-- the literal, any identifiers bound in the pattern are added to the
-- environment.
--
-- This is needed for Integer and Natural, as they can be matched with in case
-- expressions by using literal patterns or data patterns (from modules like
-- GHC.Integer.GMP.Internals and GHC.Natural).
--
ghcMatchLiteral :: Literal -> Pat -> Eval Bool
ghcMatchLiteral l = \case
  DataPat c [] [i]
    |  IntegerLiteral n <- l
    -> case n of
         S# _
           | dcTag c == 1 -> insertInt i n

         Jp# bn
           | dcTag c == 2 -> insertBigNat i bn

         Jn# bn
           | dcTag c == 3 -> insertBigNat i bn

         _ -> pure False

    |  NaturalLiteral n <- l
    -> case n of
         S# _
           | dcTag c == 1 && n >= 0 -> insertInt i n

         Jp# bn
           | dcTag c == 2 -> insertBigNat i bn

         _ -> pure False

    |  otherwise
    -> pure False

  DataPat _ _ _ -> pure False
  LitPat m -> pure (l == m)
  DefaultPat -> pure True
 where
  insertInt :: Id -> Integer -> Eval Bool
  insertInt i n =
    let val = VLit (IntLiteral n)
     in State.withState (insertLocal i (Right val)) (pure True)

  insertBigNat :: Id -> BigNat -> Eval Bool
  insertBigNat i bn = do
    tcm <- State.gets envTcMap

    -- Add the mapping i |-> VData "BigNat" [byteArray] to the environment.
    let Just integerTcName = fmap fst (splitTyConAppM integerPrimTy)
        [_, jpDc, _] = tyConDataCons (lookupUniqMap' tcm integerTcName)
        ([bnTy], _) = splitFunTys tcm (dcType jpDc)
        Just bnTcName = fmap fst (splitTyConAppM bnTy)
        [bnDc] = tyConDataCons (lookupUniqMap' tcm bnTcName)

        -- unsafeCoerce should be safe: BigNat and ByteArray are both newtype
        -- wrappers around ByteArray#.
        ba = VLit (ByteArrayLiteral (unsafeCoerce bn))
        val = VData bnDc [Left ba]

     in State.withState (insertLocal i (Right val)) (pure True)

-- | Determine whether a given pattern is matched by a data constructor. If
-- the match is successful, add any bound identifiers from the pattern to
-- the environment.
--
-- See evaluateCaseWith in Clash.Core.Evaluator.Semantics for more information.
--
ghcMatchData :: DataCon -> [Either Value Type] -> Pat -> Eval Bool
ghcMatchData dc args = \case
  DataPat c tvs ids ->
    if dc /= c then pure False else
      -- Insert bindings from pattern into the environment
      let sepArgs = partitionEithers args
          (tms, tys) = bimap (zip ids . fmap Right) (zip tvs) sepArgs
       in State.withState (insertTypes tys . insertLocals tms) (pure True)

  LitPat _   -> pure False
  DefaultPat -> pure True

