{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "GHC.Types" in the evaluator. See
  "Clash.Core.Evaluator.Primitives" for how to write them.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.Core.Evaluator.Primitives.GHC.Types
  ( primitives
  ) where

import Data.Text (Text)

import Clash.Core.Literal (Literal (..))
import Clash.Util (textNameLit)

import qualified GHC.Types

import Clash.Core.Evaluator.Primitives.Util
import Clash.Core.Evaluator.Types

primitives :: [(Text, PrimReduction)]
primitives =
  [ primStepEntry $(textNameLit 'GHC.Types.I#) $ \case
      PrimStepContext{..}
        | isSubj
        , [Lit (IntLiteral i)] <- args
        -> reduce (mkIntCLit te IntLiteral i ty)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Types.W#) $ \case
      PrimStepContext{..}
        | isSubj
        , [Lit (WordLiteral i)] <- args
        -> reduce (mkIntCLit te WordLiteral i ty)
      _ -> Nothing

  ]
