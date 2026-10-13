{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "GHC.Int" in the scoped evaluator. See
  "Clash.Core.Evaluator.Primitives" for how to write them.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.Core.Evaluator.Primitives.GHC.Int
  ( primitives
  ) where

import Data.Text (Text)
import GHC.Int

import Clash.Core.Literal (Literal (..))
import Clash.Util (textNameLit)

import Clash.Core.Evaluator.Primitives.Util
import Clash.Core.Evaluator.Types

primitives :: [(Text, PrimReduction)]
primitives =
  [ primStepEntry $(textNameLit 'GHC.Int.I8#) $ \case
      PrimStepContext{..}
        | isSubj
        , [Lit (Int8Literal i)] <- args
        -> reduce (mkIntCLit te Int8Literal i ty)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Int.I16#) $ \case
      PrimStepContext{..}
        | isSubj
        , [Lit (Int16Literal i)] <- args
        -> reduce (mkIntCLit te Int16Literal i ty)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Int.I32#) $ \case
      PrimStepContext{..}
        | isSubj
        , [Lit (Int32Literal i)] <- args
        -> reduce (mkIntCLit te Int32Literal i ty)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Int.I64#) $ \case
      PrimStepContext{..}
        | isSubj
        , [Lit (Int64Literal i)] <- args
        -> reduce (mkIntCLit te Int64Literal i ty)
      _ -> Nothing

  ]
