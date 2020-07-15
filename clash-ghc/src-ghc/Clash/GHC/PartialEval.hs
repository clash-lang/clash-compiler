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
import qualified Data.HashMap.Strict as HashMap
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

import Clash.GHC.PartialEval.Bit
import Clash.GHC.PartialEval.BitVector
import Clash.GHC.PartialEval.ByteArray
import Clash.GHC.PartialEval.Char
import Clash.GHC.PartialEval.Double
import Clash.GHC.PartialEval.Enum
import Clash.GHC.PartialEval.Float
import Clash.GHC.PartialEval.GhcMisc
import Clash.GHC.PartialEval.Index
import Clash.GHC.PartialEval.Int
import Clash.GHC.PartialEval.Integer
import Clash.GHC.PartialEval.Internal
import Clash.GHC.PartialEval.Narrowing
import Clash.GHC.PartialEval.Natural
import Clash.GHC.PartialEval.Promoted
import Clash.GHC.PartialEval.Signed
import Clash.GHC.PartialEval.Transformations
import Clash.GHC.PartialEval.Unsigned
import Clash.GHC.PartialEval.Vector
import Clash.GHC.PartialEval.Word

import Clash.Debug -- TODO

-- | An evaluator for partial evaluation that uses GHC specific details. This
-- allows the evaluator to be implemented with an understanding of built-in
-- GHC types and primitives which can appear in Clash core when using GHC as a
-- compiler front-end.
--
ghcEvaluator :: Evaluator
ghcEvaluator = evaluatorWith ghcMatchLiteral ghcMatchData ghcEvaluatePrim

-- | Evaluate a primitive using the specified primitive implementations.
-- If a primitive can't be reduced, we do the next best thing and force all
-- it's arguments to WHNF. This is done to simplify the definition of the
-- NePrim constructor in Neutral.
--
ghcEvaluatePrim :: PrimInfo -> [Either Term Type] -> Eval Value
ghcEvaluatePrim p args = do
  -- traceM (show (primName p) <> ": " <> show args)

  case HashMap.lookup (primName p) primImpls of
    Just f -> do
      res <- runPrimEval (f ghcEvaluator p args) neuPrim
      -- traceM (show (primName p) <> ": result " <> show res)
      pure res

    -- TODO This should ideally warn that a primitive has no implementation.
    Nothing ->
      case primCoreId p of
        Just c  -> do
          -- traceM (show (primName p) <> ": falling back to core")
          evaluateWhnf ghcEvaluator (mkApps (Var c) args)

        Nothing -> do
          -- traceM (show (primName p) <> ": no implementation")
          neuPrim
 where
  neuPrim = do
    evalArgs <- traverse (bitraverse (evaluateWhnf ghcEvaluator) pure) args
    pure (VNeu (NePrim p evalArgs))

  primImpls = HashMap.unions
    [ bitPrims
    , bitVectorPrims
    , byteArrayPrims
    , charPrims
    , doublePrims
    , enumPrims
    , floatPrims
    , ghcPrims
    , indexPrims
    , intPrims
    , integerPrims
    , narrowingPrims
    , naturalPrims
    , promotedPrims
    , signedPrims
    , transformationsPrims
    , unsignedPrims
    , vectorPrims
    , wordPrims
    ]

-- | Attempt to match a literal against a pattern. If the pattern matches
-- the literal, any identifiers bound in the pattern are added to the
-- environment.
--
-- This is needed for Integer and Natural, as they can be matched with in case
-- expressions by using literal patterns or data patterns (from modules like
-- GHC.Integer.GMP.Internals and GHC.Natural).
--
ghcMatchLiteral :: TyConMap -> Literal -> Alt -> PatResult
ghcMatchLiteral tcm l alt@(pat, _) =
  case pat of
    DataPat c [] [i]
      |  IntegerLiteral n <- l
      -> case n of
           S# _
             | dcTag c == 1 -> insertInt i n

           Jp# bn
             | dcTag c == 2 -> insertBigNat i bn

           Jn# bn
             | dcTag c == 3 -> insertBigNat i bn

           _ -> NoMatch

      |  NaturalLiteral n <- l
      -> case n of
           S# _
             | dcTag c == 1 && n >= 0 -> insertWord i n

           Jp# bn
             | dcTag c == 2 -> insertBigNat i bn

           _ -> NoMatch

      |  otherwise
      -> NoMatch

    LitPat m
      |  l == m
      -> Match alt [] []

    DefaultPat
      -> Match alt [] []

    _ -> NoMatch
 where
  insertInt :: Id -> Integer -> PatResult
  insertInt i n = Match alt [] [(i, Literal (IntLiteral n))]

  insertWord :: Id -> Integer -> PatResult
  insertWord i n = Match alt [] [(i, Literal (WordLiteral n))]

  insertBigNat :: Id -> BigNat -> PatResult
  insertBigNat i bn =
    Match alt [] [(i, val)]
   where
    -- Inspect the type of Jp# to get the DataCon for BigNat.
    -- Somewhat of a hack but I can't think of a better way - Alex.
    Just integerTcName = fmap fst (splitTyConAppM integerPrimTy)
    [_, jpDc, _] = tyConDataCons (lookupUniqMap' tcm integerTcName)
    ([bnTy], _) = splitFunTys tcm (dcType jpDc)
    Just bnTcName = fmap fst (splitTyConAppM bnTy)
    [bnDc] = tyConDataCons (lookupUniqMap' tcm bnTcName)

    -- unsafeCoerce should be safe: BigNat and ByteArray are both newtype
    -- wrappers around ByteArray#.
    ba = Literal (ByteArrayLiteral (unsafeCoerce bn))
    val = Data bnDc `App` ba

-- | Determine whether a given pattern is matched by a data constructor. If
-- the match is successful, add any bound identifiers from the pattern to
-- the environment.
--
-- See evaluateCaseWith in Clash.Core.Evaluator.Semantics for more information.
--
ghcMatchData :: DataCon -> [Either Term Type] -> Alt -> PatResult
ghcMatchData dc args alt@(pat, _) =
  case pat of
    DataPat c tvs ids
      |  dc == c
      -> let (tms, tys) = bimap (zip ids) (zip tvs) (partitionEithers args)
          in Match alt tys tms

    DefaultPat -> Match alt [] []
    _ -> NoMatch

