{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "GHC.Num.Natural" in the scoped evaluator.
  See "Clash.Core.Evaluator.Primitives" for how to write them.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Evaluator.Primitives.GHC.Num.Natural
  ( primitives
  ) where

import Data.Text (Text)
import GHC.Num.Natural (naturalSubUnsafe)

import Clash.Core.Literal (Literal (..))
import Clash.Util (textNameLit)

import Clash.Core.Evaluator.Primitives.Util
import Clash.Core.Evaluator.Types
import Clash.Core.Syntax

primitives :: [(Text, PrimReduction)]
primitives =
  [ primStepEntry $(textNameLit 'GHC.Num.Natural.naturalSubUnsafe) $ \case
      PrimStepContext{..}
        | Just (i,j) <- naturalLiterals args
        ->
         let nTy = getResultTy te sc ty tys in
         reduce (checkNaturalRange2 nTy i j naturalSubUnsafe)
      _ -> Nothing

  , primStepEntry "GHC.Num.Natural.$wnaturalSignum" $ \case
      PrimStepContext{..}
        | [i] <- naturalLiterals' args
        -> reduce (Literal (WordLiteral (signum i)))
      _ -> Nothing

  ]
