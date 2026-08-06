{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE MagicHash #-}
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

import           Clash.Core.Evaluator.Types
import           Clash.Core.Literal  (Literal (..))
import Clash.Core.Term (Term (..), mkApps)
import Clash.Core.Type (TypeView (..), splitFunForallTy, tyView)
import Clash.Core.TyCon (tyConDataCons)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Util (flogBase, textNameLit)

import qualified GHC.Num

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
  [ ( $(textNameLit 'GHC.Num.naturalLogBase#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (a,b) <- naturalLiterals args
            , Just c <- flogBase a b
            -> (reduce . Literal . WordLiteral . toInteger) c
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.NS)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (WordLiteral w)] <- args
            -> reduce (Literal (NaturalLiteral w))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Num.NB)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (ByteArrayLiteral (BA.ByteArray ba))] <- args
            -> reduce (Literal (NaturalLiteral (IP ba)))
            | [Lit l] <- args
            -> error ("NB: " <> show l)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalAdd)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange2 nTy i j (+))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalMul)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange2 nTy i j (*))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalSubThrow)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange nTy [i, j] (\[i', j'] ->
                        case minusNaturalMaybe i' j' of
                          Nothing -> checkNaturalRange1 nTy (-1) id
                          Just n -> naturalToNaturalLiteral n))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalFromWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (WordLiteral w)] <- args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange1 nTy w id)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalToWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- naturalLiterals' args
            -> reduce (integerToWordLiteral i)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalQuot)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange2 nTy i j quot)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalRem)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange2 nTy i j rem)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalQuotRem#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
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
    )

  , ( $(textNameLit 'GHC.Num.naturalGcd)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange2 nTy i j gcd)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalLcm)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange2 nTy i j lcm)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalGt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            -> reduce (boolToIntLiteral (i > j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalGe#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            -> reduce (boolToIntLiteral (i >= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalEq#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            -> reduce (boolToIntLiteral (i == j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalNe#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            -> reduce (boolToIntLiteral (i /= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalLt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            -> reduce (boolToIntLiteral (i < j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalLe#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            -> reduce (boolToIntLiteral (i <= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalShiftL#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [iV, Lit (WordLiteral j)] <- args
            , [i] <- naturalLiterals' [iV]
            -> reduce (naturalToNaturalLiteral (fromInteger (i `shiftL` fromInteger j)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalShiftR#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [iV, Lit (WordLiteral j)] <- args
            , [i] <- naturalLiterals' [iV]
            -> reduce (naturalToNaturalLiteral (fromInteger (i `shiftR` fromInteger j)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalCompare)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
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
    )

  , ( $(textNameLit 'GHC.Num.naturalSignum)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- naturalLiterals' args
            -> reduce (Literal (NaturalLiteral (signum i)))
          _ -> Nothing
    )
  ]
