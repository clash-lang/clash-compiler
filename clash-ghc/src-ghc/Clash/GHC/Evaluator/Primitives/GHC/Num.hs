{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Clash.GHC.Evaluator.Primitives.GHC.Num
  ( primitives
  ) where

import           Data.Bits
import qualified Data.Primitive.ByteArray as BA
import           Data.Text           (Text)
import GHC.Integer (compareInteger)
import GHC.Num.Integer (Integer (..))
import           GHC.Natural
import           GHC.Word (Word(W#))

import           Clash.Core.Evaluator.Types
import           Clash.Core.Literal  (Literal (..))
import Clash.Core.Term (Term (..), mkApps)
import Clash.Core.Type (TypeView (..), splitFunForallTy, tyView)
import Clash.Core.TyCon (tyConDataCons)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Util (textNameLit)

import qualified GHC.Num

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
  [ primStepEntry $(textNameLit 'GHC.Num.naturalLogBase#) $ \case
      PrimStepContext{..}
        | Just (a,b) <- naturalLiterals args
        , a > 1
        -> reduce $ catchErrorCall
             (Literal (WordLiteral (toInteger
               (W# (GHC.Num.naturalLogBase# (fromInteger a) (fromInteger b))))))
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
         let nTy = snd (splitFunForallTy ty) in
         reduce (checkNaturalRange2 nTy i j (+))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalMul) $ \case
      PrimStepContext{..}
        | Just (i,j) <- naturalLiterals args
        ->
         let nTy = snd (splitFunForallTy ty) in
         reduce (checkNaturalRange2 nTy i j (*))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalSubThrow) $ \case
      PrimStepContext{..}
        | Just (i,j) <- naturalLiterals args
        ->
         let nTy = snd (splitFunForallTy ty) in
         reduce (checkNaturalRange nTy [i, j] (\[i', j'] ->
                    case minusNaturalMaybe i' j' of
                      Nothing -> checkNaturalRange1 nTy (-1) id
                      Just n -> naturalToNaturalLiteral n))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalFromWord#) $ \case
      PrimStepContext{..}
        | [Lit (WordLiteral w)] <- args
        ->
         let nTy = snd (splitFunForallTy ty) in
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
         let nTy = snd (splitFunForallTy ty) in
         reduce (checkNaturalRange2 nTy i j quot)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalRem) $ \case
      PrimStepContext{..}
        | Just (i,j) <- naturalLiterals args
        ->
         let nTy = snd (splitFunForallTy ty) in
         reduce (checkNaturalRange2 nTy i j rem)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalQuotRem#) $ \case
      PrimStepContext{..} -- :: Natural -> Natural -> (#Natural, Natural#)
        | [i, j] <- naturalLiterals' args
        -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
               (Just tupTc) = UniqMap.lookup tupTcNm tcm
               [tupDc] = tyConDataCons tupTc
               (q,r) = quotRem (fromInteger i) (fromInteger j)
        in reduce $
             mkApps (Data tupDc) (map Right tyArgs ++
                    [ Left $ catchDivByZero (naturalToNaturalLiteral q)
                    , Left $ catchDivByZero (naturalToNaturalLiteral r)])
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalGcd) $ \case
      PrimStepContext{..}
        | Just (i,j) <- naturalLiterals args
        ->
         let nTy = snd (splitFunForallTy ty) in
         reduce (checkNaturalRange2 nTy i j gcd)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalLcm) $ \case
      PrimStepContext{..}
        | Just (i,j) <- naturalLiterals args
        ->
         let nTy = snd (splitFunForallTy ty) in
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
        -> let -- Get the required result type (viewed as an applied type constructor name)
               (_,tyView -> TyConApp tupTcNm []) = splitFunForallTy ty
               -- Find the type constructor from the name
               (Just tupTc) = UniqMap.lookup tupTcNm tcm
               -- Get the data constructors of that type
               -- The type is 'Ordering', so they are: 'LT', 'EQ', 'GT'
               [ltDc, eqDc, gtDc] = tyConDataCons tupTc
               -- Do the actual compile-time evaluation
               ordVal = compareInteger i j
            in reduce $ case ordVal of
                LT -> Data ltDc
                EQ -> Data eqDc
                GT -> Data gtDc
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.naturalSignum) $ \case
      PrimStepContext{..}
        | [i] <- naturalLiterals' args
        -> reduce (Literal (NaturalLiteral (signum i)))
      _ -> Nothing

  ]
