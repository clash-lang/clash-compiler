{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "GHC.Num.Integer" in the scoped evaluator:
  ports of @Clash.GHC.Evaluator.Primitives.GHC.Num.Integer@. See
  "Clash.Core.Scoped.Evaluator.Primitives" for how to port them.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Scoped.Evaluator.Primitives.GHC.Num.Integer
  ( primitives
  ) where

import Data.Text (Text)

import Clash.Util (textNameLit)

import qualified GHC.Num.Integer

import Clash.Core.Scoped.Evaluator.Primitives.Util
import Clash.Core.Scoped.Evaluator.Types

primitives :: [(Text, PrimReduction)]
primitives =
  [ primStepEntry $(textNameLit 'GHC.Num.Integer.integerQuotRem#) $ \case
      PrimStepContext{..} -- :: Integer -> Integer -> (#Integer, Integer#)
        | [i, j] <- integerLiterals' args
        -> let (q,r) = quotRem i j
        in reduce $ mkResultCon te sc ty tys
             [ catchDivByZero (integerToIntegerLiteral q)
             , catchDivByZero (integerToIntegerLiteral r) ]
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerAdd) $ \case
      PrimStepContext{..}
        | Just (i,j) <- integerLiterals args
        -> reduce (integerToIntegerLiteral (i+j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerSub) $ \case
      PrimStepContext{..}
        | Just (i,j) <- integerLiterals args
        -> reduce (integerToIntegerLiteral (i-j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerMul) $ \case
      PrimStepContext{..}
        | Just (i,j) <- integerLiterals args
        -> reduce (integerToIntegerLiteral (i*j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerEq) $ \case
      PrimStepContext{..}
        | Just (i,j) <- integerLiterals args
        -> reduce (boolToBoolLiteral te ty (i == j))
      _ -> Nothing
  ]
