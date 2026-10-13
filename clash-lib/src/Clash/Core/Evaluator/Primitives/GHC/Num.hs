{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "GHC.Num" in the scoped evaluator. See
  "Clash.Core.Evaluator.Primitives" for how to write them.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.Core.Evaluator.Primitives.GHC.Num
  ( primitives
  ) where

import Data.Bits
import qualified Data.Primitive.ByteArray as BA
import Data.Text (Text)
import GHC.Integer (compareInteger)
import GHC.Num.Integer (Integer (..))
import GHC.Natural

import Clash.Core.Literal (Literal (..))
import Clash.Util (curLoc, flogBase, textNameLit)

import qualified GHC.Num

import Clash.Core.Evaluator.Primitives.Util
import Clash.Core.Evaluator.Types
import Clash.Core.Syntax

primitives :: [(Text, PrimReduction)]
primitives =
  [ primStepEntry $(textNameLit 'GHC.Num.naturalLogBase#) $ \case
      PrimStepContext{..}
        | Just (a,b) <- naturalLiterals args
        , Just c <- flogBase a b
        -> (reduce . Literal . WordLiteral . toInteger) c
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.NS) $ \case
      PrimStepContext{..}
        | [Lit (WordLiteral w)] <- args
        -> reduce (Literal (NaturalLiteral w))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.NB) $ \case
      PrimStepContext{..}
        | [Lit (ByteArrayLiteral (BA.ByteArray ba))] <- args
        -> reduce (Literal (NaturalLiteral (IP ba)))
        | [Lit l] <- args
        -> error ("NB: " <> show l)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalAdd) $ \case
      PrimStepContext{..}
        | Just (i,j) <- naturalLiterals args
        ->
         let nTy = getResultTy te sc ty tys in
         reduce (checkNaturalRange2 nTy i j (+))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalMul) $ \case
      PrimStepContext{..}
        | Just (i,j) <- naturalLiterals args
        ->
         let nTy = getResultTy te sc ty tys in
         reduce (checkNaturalRange2 nTy i j (*))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalSubThrow) $ \case
      PrimStepContext{..}
        | Just (i,j) <- naturalLiterals args
        ->
         let nTy = getResultTy te sc ty tys in
         reduce (checkNaturalRange nTy [i, j] (\case
                    [i', j'] -> case minusNaturalMaybe i' j' of
                      Nothing -> checkNaturalRange1 nTy (-1) id
                      Just n -> naturalToNaturalLiteral n
                    _ -> error ($(curLoc) ++ "unreachable")))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalFromWord#) $ \case
      PrimStepContext{..}
        | [Lit (WordLiteral w)] <- args
        ->
         let nTy = getResultTy te sc ty tys in
         reduce (checkNaturalRange1 nTy w id)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalToWord#) $ \case
      PrimStepContext{..}
        | [i] <- naturalLiterals' args
        -> reduce (integerToWordLiteral i)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalQuot) $ \case
      PrimStepContext{..}
        | Just (i,j) <- naturalLiterals args
        ->
         let nTy = getResultTy te sc ty tys in
         reduce (checkNaturalRange2 nTy i j quot)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalRem) $ \case
      PrimStepContext{..}
        | Just (i,j) <- naturalLiterals args
        ->
         let nTy = getResultTy te sc ty tys in
         reduce (checkNaturalRange2 nTy i j rem)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalQuotRem#) $ \case
      PrimStepContext{..} -- :: Natural -> Natural -> (#Natural, Natural#)
        | [i, j] <- naturalLiterals' args
        -> let (q,r) = quotRem (fromInteger i) (fromInteger j)
        in reduce $ mkResultCon te sc ty tys
             [ catchDivByZero (naturalToNaturalLiteral q)
             , catchDivByZero (naturalToNaturalLiteral r) ]
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalGcd) $ \case
      PrimStepContext{..}
        | Just (i,j) <- naturalLiterals args
        ->
         let nTy = getResultTy te sc ty tys in
         reduce (checkNaturalRange2 nTy i j gcd)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalLcm) $ \case
      PrimStepContext{..}
        | Just (i,j) <- naturalLiterals args
        ->
         let nTy = getResultTy te sc ty tys in
         reduce (checkNaturalRange2 nTy i j lcm)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalGt#) $ \case
      PrimStepContext{..}
        | Just (i,j) <- naturalLiterals args
        -> reduce (boolToIntLiteral (i > j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalGe#) $ \case
      PrimStepContext{..}
        | Just (i,j) <- naturalLiterals args
        -> reduce (boolToIntLiteral (i >= j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalEq#) $ \case
      PrimStepContext{..}
        | Just (i,j) <- naturalLiterals args
        -> reduce (boolToIntLiteral (i == j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalNe#) $ \case
      PrimStepContext{..}
        | Just (i,j) <- naturalLiterals args
        -> reduce (boolToIntLiteral (i /= j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalLt#) $ \case
      PrimStepContext{..}
        | Just (i,j) <- naturalLiterals args
        -> reduce (boolToIntLiteral (i < j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalLe#) $ \case
      PrimStepContext{..}
        | Just (i,j) <- naturalLiterals args
        -> reduce (boolToIntLiteral (i <= j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalShiftL#) $ \case
      PrimStepContext{..}
        | [iV, Lit (WordLiteral j)] <- args
        , [i] <- naturalLiterals' [iV]
        -> reduce (naturalToNaturalLiteral (fromInteger (i `shiftL` fromInteger j)))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalShiftR#) $ \case
      PrimStepContext{..}
        | [iV, Lit (WordLiteral j)] <- args
        , [i] <- naturalLiterals' [iV]
        -> reduce (naturalToNaturalLiteral (fromInteger (i `shiftR` fromInteger j)))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalCompare) $ \case
      PrimStepContext{..}
        | [i, j] <- naturalLiterals' args
        -> reduce (orderingToOrderingLiteral te ty (compareInteger i j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalSignum) $ \case
      PrimStepContext{..}
        | [i] <- naturalLiterals' args
        -> reduce (Literal (NaturalLiteral (signum i)))
      _ -> Nothing

  ]
