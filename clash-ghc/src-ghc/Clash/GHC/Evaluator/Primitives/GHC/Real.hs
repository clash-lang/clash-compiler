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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.GHC.Evaluator.Primitives.GHC.Real
  ( primitives
  ) where

import           Data.Text           (Text)

import           Clash.Core.Evaluator.Types
import           Clash.Core.Literal  (Literal (..))

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
  -- GHC.Real.^  -- XXX: Very fragile
  --   ^_f, $wf, $wf1 are specializations of the internal function f in the implementation of (^) in GHC.Real
  [ primStepEntry "GHC.Real.^_f" $ \case
      PrimStepContext{..}  -- :: Integer -> Integer -> Integer
        | [i,j] <- integerLiterals' args
        -> reduce (catchErrorCall (integerToIntegerLiteral $ i ^ j))
      _ -> Nothing

  , primStepEntry "GHC.Real.$wf" $ \case
      PrimStepContext{..}  -- :: Integer -> Int# -> Integer
        | [iV, Lit (IntLiteral j)] <- args
        , [i] <- integerLiterals' [iV]
        -> reduce (catchErrorCall (integerToIntegerLiteral $ i ^ j))
      _ -> Nothing

  , primStepEntry "GHC.Real.$wf1" $ \case
      PrimStepContext{..} -- :: Int# -> Int# -> Int#
        | [Lit (IntLiteral i), Lit (IntLiteral j)] <- args
        -> reduce (catchErrorCall (integerToIntLiteral $ i ^ j))
      _ -> Nothing

  -- Which specialization each worker name implements shifts between GHC
  -- versions, so they all share one shape-dispatching implementation. See
  -- 'powImplWorker'.
  , primStepEntry "GHC.Real.^_$s$spowImpl" powImplWorker
  , primStepEntry "GHC.Real.^_$s$spowImpl1" powImplWorker
  , primStepEntry "GHC.Real.^_$s$spowImpl2" powImplWorker
  , primStepEntry "GHC.Real.$w$spowImpl" powImplWorker
  , primStepEntry "GHC.Real.$w$spowImpl1" powImplWorker

  , primStepEntry "GHC.Real.^_$sf2" $ \case
      PrimStepContext{..} -- :: Int# -> Integer -> Integer
        | [intLiteral -> Just j, integerLiteral -> Just i] <- args
        -> reduce (catchErrorCall (integerToIntLiteral $ i ^ j))
      _ -> Nothing

  ]
