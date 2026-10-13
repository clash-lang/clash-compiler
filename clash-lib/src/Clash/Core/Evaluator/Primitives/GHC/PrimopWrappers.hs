{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "GHC.PrimopWrappers" in the
  evaluator. See "Clash.Core.Evaluator.Primitives" for how to write
  them.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Evaluator.Primitives.GHC.PrimopWrappers
  ( primitives
  ) where

import Data.Text (Text)

import Clash.Core.Evaluator.Types

#if MIN_VERSION_ghc_prim(0,12,0)
import Clash.Core.DataCon (DataCon (..))
import Clash.Core.Literal (Literal (..))
import Clash.Util (textNameLit)

import qualified GHC.PrimopWrappers

import Clash.Core.Evaluator.Primitives.Util
import Clash.Core.Syntax
#endif

primitives :: [(Text, PrimReduction)]
#if MIN_VERSION_ghc_prim(0,12,0)
primitives =
  [ primStepEntry $(textNameLit 'GHC.PrimopWrappers.dataToTagSmall#) $ \case
      PrimStepContext{..}
        | [DC dc _] <- args
        -> reduce (Literal (IntLiteral (toInteger (dcTag dc - 1))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.PrimopWrappers.dataToTagLarge#) $ \case
      PrimStepContext{..}
        | [DC dc _] <- args
        -> reduce (Literal (IntLiteral (toInteger (dcTag dc - 1))))
      _ -> Nothing

  ]
#else
primitives = []
#endif
