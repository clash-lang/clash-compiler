{-|
  Copyright     : (C) 2020, QBayLogic B.V.
  License       : BSD2 (see the file LICENSE)
  Maintainer    : QBayLogic B.V. <devops@qbaylogic.com>

This module contains the implementation of partial evaluation that is
used by the GHC frontend.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}

module Clash.GHC.PartialEval
  ( ghcEvaluator
  ) where

import Data.Bifunctor
import Data.Bitraversable
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

-- | An evaluator for partial evaluation that uses GHC specific details. This
-- allows the evaluator to be implemented with an understanding of built-in
-- GHC types and primitives which can appear in Clash core when using GHC as a
-- compiler front-end.
--
ghcEvaluator :: Evaluator
ghcEvaluator = evaluatorWith ghcMatchLiteral ghcMatchData ghcEvaluatePrim

-- TODO Implement evaluation for primitives and call here.
--
-- If a primitive can't be reduced, we do the next best thing and force all
-- it's arguments to WHNF. This is done to simplify the definition of the
-- NePrim constructor in Neutral.
--
ghcEvaluatePrim :: PrimInfo -> [Either Term Type] -> Eval Value
ghcEvaluatePrim p args =
  VNeu . NePrim p <$> forceArgs args
 where
  forceArgs = traverse (bitraverse (evaluateWhnf ghcEvaluator) pure)

-- | Attempt to match a literal against a pattern. If the pattern matches
-- the literal, any identifiers bound in the pattern are added to the
-- environment.
--
-- This is needed for Integer and Natural, as they can be matched with in case
-- expressions by using literal patterns or data patterns (from modules like
-- GHC.Integer.GMP.Internals and GHC.Natural).
--
ghcMatchLiteral :: Literal -> Pat -> Eval PatResult
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

         _ -> pure NoMatch

    |  NaturalLiteral n <- l
    -> case n of
         S# _
           | dcTag c == 1 && n >= 0 -> insertInt i n

         Jp# bn
           | dcTag c == 2 -> insertBigNat i bn

         _ -> pure NoMatch

    |  otherwise
    -> pure NoMatch

  LitPat m
    |  l == m
    -> pure (Match [] [])

  DefaultPat
    -> pure (Match [] [])

  _ -> pure NoMatch
 where
  insertInt :: Id -> Integer -> Eval PatResult
  insertInt i n = pure (Match [] [(i, Literal (IntLiteral n))])

  insertBigNat :: Id -> BigNat -> Eval PatResult
  insertBigNat i bn = do
    tcm <- getTyConMap

    -- Add the mapping i |-> VData "BigNat" [byteArray] to the environment.
    let Just integerTcName = fmap fst (splitTyConAppM integerPrimTy)
        [_, jpDc, _] = tyConDataCons (lookupUniqMap' tcm integerTcName)
        ([bnTy], _) = splitFunTys tcm (dcType jpDc)
        Just bnTcName = fmap fst (splitTyConAppM bnTy)
        [bnDc] = tyConDataCons (lookupUniqMap' tcm bnTcName)

        -- unsafeCoerce should be safe: BigNat and ByteArray are both newtype
        -- wrappers around ByteArray#.
        ba = Literal (ByteArrayLiteral (unsafeCoerce bn))
        val = Data bnDc `App` ba

     in pure (Match [] [(i, val)])

-- | Determine whether a given pattern is matched by a data constructor. If
-- the match is successful, add any bound identifiers from the pattern to
-- the environment.
--
-- See evaluateCaseWith in Clash.Core.Evaluator.Semantics for more information.
--
ghcMatchData :: DataCon -> [Either Term Type] -> Pat -> Eval PatResult
ghcMatchData dc args = \case
  DataPat c tvs ids
    |  dc == c
    ,  (tms, tys) <- bimap (zip ids) (zip tvs) (partitionEithers args)
    -> pure (Match tys tms)

  DefaultPat
    -> pure (Match [] [])

  _ -> pure NoMatch

