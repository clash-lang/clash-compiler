{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "GHC.Classes" in the scoped evaluator. See
  "Clash.Core.Evaluator.Primitives" for how to write them.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Evaluator.Primitives.GHC.Classes
  ( primitives
  ) where

import Data.Text (Text)

import Clash.Core.Literal (Literal (..))
import qualified Clash.Normalize.Primitives as NP
import Clash.Util (textNameLit)

import qualified GHC.Classes

import Clash.Core.Evaluator.Primitives.Util
import Clash.Core.Evaluator.Types
import Clash.Core.Syntax

primitives :: [(Text, PrimReduction)]
primitives =
  [ primStepEntry $(textNameLit 'GHC.Classes.eqInt) $ \case
      PrimStepContext{..} | Just (i,j) <- intCLiterals args
        -> reduce (boolToBoolLiteral te ty (i == j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Classes.neInt) $ \case
      PrimStepContext{..} | Just (i,j) <- intCLiterals args
        -> reduce (boolToBoolLiteral te ty (i /= j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Classes.leInt) $ \case
      PrimStepContext{..} | Just (i,j) <- intCLiterals args
        -> reduce (boolToBoolLiteral te ty (i <= j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Classes.ltInt) $ \case
      PrimStepContext{..} | Just (i,j) <- intCLiterals args
        -> reduce (boolToBoolLiteral te ty (i < j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Classes.geInt) $ \case
      PrimStepContext{..} | Just (i,j) <- intCLiterals args
        -> reduce (boolToBoolLiteral te ty (i >= j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Classes.gtInt) $ \case
      PrimStepContext{..} | Just (i,j) <- intCLiterals args
        -> reduce (boolToBoolLiteral te ty (i > j))
      _ -> Nothing

  -- Lazy in both arguments: evaluates both with nested machines. The second
  -- one runs in the machine of the first, and the result continues in the
  -- second.
  , primStepEntry $(textNameLit '(GHC.Classes.&&)) (andOr True)

  , primStepEntry $(textNameLit '(GHC.Classes.||)) (andOr False)

  , primStepEntry $(textNameLit 'GHC.Classes.divInt#) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals args
        -> reduce (catchDivByZero (integerToIntLiteral (i `div` j)))
      _ -> Nothing

  -- modInt# :: Int# -> Int# -> Int#
  , primStepEntry $(textNameLit 'GHC.Classes.modInt#) $ \case
      PrimStepContext{..}
        | [dividend, divisor] <- intLiterals' args
        ->
          if divisor == 0 then
            let iTy = getResultTy te sc ty tys in
            reduce (TyApp (Prim NP.undefined) iTy)
          else
            reduce (Literal (IntLiteral (dividend `mod` divisor)))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Classes.not) $ \case
      PrimStepContext{..}
        | [boolLiteral -> Just b] <- args
        -> reduce (boolToBoolLiteral te ty (not b))
      _ -> Nothing
  ]

-- | @(&&)@ and @(||)@
andOr :: Distinct n => Bool -> PrimStepContext o n -> Maybe (Next o n)
andOr isAnd PrimStepContext{..}
  | [lArg, rArg] <- args
  = whnfArg (valToTerm lArg) $ \mach1 lArgWHNF ->
      whnfIn mach1 (sink (valToTerm rArg)) $ \mach2 rW ->
        let lW = sink lArgWHNF
            -- Continue in the machine that evaluated the arguments: their
            -- WHNFs can refer to heap bindings made there
            continue = reduceWith mach2
            lit = boolToBoolLiteral te ty
            -- The constructor that decides the result on its own
            absorbing con = if isAnd then isFalseDC con else isTrueDC con
            combine l r = if isAnd then l && r else l || r
        in case (lW, rW) of
             (Data lCon, Data rCon) ->
               continue (lit (combine (isTrueDC lCon) (isTrueDC rCon)))
             (Data lCon, _)
               | absorbing lCon -> continue (lit (not isAnd))
               | otherwise -> continue rW
             (_, Data rCon)
               | absorbing rCon -> continue (lit (not isAnd))
               | otherwise -> continue lW
             _ -> Nothing
  | otherwise
  = Nothing
