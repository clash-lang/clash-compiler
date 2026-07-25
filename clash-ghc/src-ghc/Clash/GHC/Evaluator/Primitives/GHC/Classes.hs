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

module Clash.GHC.Evaluator.Primitives.GHC.Classes
  ( primitives
  ) where

import           Data.Text           (Text)
import           Data.Text.Extra     (showt)

import           Clash.Core.DataCon  (DataCon (..))
import           Clash.Core.Evaluator.Types
import           Clash.Core.Literal  (Literal (..))
import Clash.Core.Name (Name (..))
import Clash.Core.Term (Term (..))
import Clash.Core.Type (splitFunForallTy)
import Clash.Util (textNameLit)

import qualified Clash.Normalize.Primitives as NP

import {-# SOURCE #-} Clash.GHC.Evaluator

import qualified GHC.Classes

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
  [ ( $(textNameLit 'GHC.Classes.eqInt)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intCLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i == j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Classes.neInt)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intCLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i /= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Classes.leInt)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intCLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i <= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Classes.ltInt)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intCLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i < j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Classes.geInt)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intCLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i >= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Classes.gtInt)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intCLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i > j))
          _ -> Nothing
    )

  , ( $(textNameLit '(GHC.Classes.&&))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [ lArg , rArg ] <- args
            , eval <- evaluator
            -- evaluation of the arguments is deferred until the evaluation of the ghcPrimUnwindWith
            -- to make `&&` lazy in both arguments
            , mach1@Machine{mStack=[],mTerm=lArgWHNF} <- whnf eval tcm True (setTerm (valToTerm lArg) $ stackClear mach)
            , mach2@Machine{mStack=[],mTerm=rArgWHNF} <- whnf eval tcm True (setTerm (valToTerm rArg) $ stackClear mach1)
            -> case [ lArgWHNF, rArgWHNF ] of
                 [ Data lCon, Data rCon ] ->
                   Just $ mach2
                     { mStack = mStack mach
                     , mTerm = boolToBoolLiteral tcm ty (isTrueDC lCon && isTrueDC rCon)
                     }

                 [ Data lCon, _ ]
                   | isTrueDC lCon -> reduce rArgWHNF
                   | otherwise     -> reduce (boolToBoolLiteral tcm ty False)

                 [ _, Data rCon ]
                   | isTrueDC rCon -> reduce lArgWHNF
                   | otherwise     -> reduce (boolToBoolLiteral tcm ty False)

                 _ -> Nothing
          _ -> Nothing
    )

  , ( $(textNameLit '(GHC.Classes.||))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [ lArg , rArg ] <- args
            , eval <- evaluator
            -- evaluation of the arguments is deferred until the evaluation of the ghcPrimUnwindWith
            -- to make `||` lazy in both arguments
            , mach1@Machine{mStack=[],mTerm=lArgWHNF} <- whnf eval tcm True (setTerm (valToTerm lArg) $ stackClear mach)
            , mach2@Machine{mStack=[],mTerm=rArgWHNF} <- whnf eval tcm True (setTerm (valToTerm rArg) $ stackClear mach1)
            -> case [ lArgWHNF, rArgWHNF ] of
                 [ Data lCon, Data rCon ] ->
                   Just $ mach2
                     { mStack = mStack mach
                     , mTerm = boolToBoolLiteral tcm ty (isTrueDC lCon || isTrueDC rCon)
                     }

                 [ Data lCon, _ ]
                   | isFalseDC lCon -> reduce rArgWHNF
                   | otherwise      -> reduce (boolToBoolLiteral tcm ty True)

                 [ _, Data rCon ]
                   | isFalseDC rCon -> reduce lArgWHNF
                   | otherwise      -> reduce (boolToBoolLiteral tcm ty True)

                 _ -> Nothing
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Classes.divInt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce (catchDivByZero (integerToIntLiteral (i `div` j)))
          _ -> Nothing
    )

  -- modInt# :: Int# -> Int# -> Int#
  , ( $(textNameLit 'GHC.Classes.modInt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [dividend, divisor] <- intLiterals' args
            ->
              if divisor == 0 then
                let iTy = snd (splitFunForallTy ty) in
                reduce (TyApp (Prim NP.undefined) iTy)
              else
                reduce (Literal (IntLiteral (dividend `mod` divisor)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Classes.not)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [DC bCon _] <- args
            -> reduce (boolToBoolLiteral tcm ty (nameOcc (dcName bCon) == showt 'False))
          _ -> Nothing
    )
  ]
