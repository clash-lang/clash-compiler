{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "GHC.Word" in the scoped evaluator: ports of
  @Clash.GHC.Evaluator.Primitives.GHC.Word@. See
  "Clash.Core.Scoped.Evaluator.Primitives" for how to port them.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.Core.Scoped.Evaluator.Primitives.GHC.Word
  ( primitives
  ) where

import Data.Text (Text)
import GHC.Word

import Clash.Core.Literal (Literal (..))
import Clash.Util (textNameLit)

import Clash.Core.Scoped.Evaluator.Primitives.Util
import Clash.Core.Scoped.Evaluator.Types

primitives :: [(Text, PrimReduction)]
primitives =
  [ primStepEntry $(textNameLit 'GHC.Word.W8#) $ \case
      PrimStepContext{..}
        | isSubj
        , [Lit (Word8Literal c)] <- args
        -> reduce (mkIntCLit te Word8Literal c ty)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Word.W16#) $ \case
      PrimStepContext{..}
        | isSubj
        , [Lit (Word16Literal c)] <- args
        -> reduce (mkIntCLit te Word16Literal c ty)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Word.W32#) $ \case
      PrimStepContext{..}
        | isSubj
        , [Lit (Word32Literal c)] <- args
        -> reduce (mkIntCLit te Word32Literal c ty)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Word.W64#) $ \case
      PrimStepContext{..}
        | isSubj
        , [Lit (Word64Literal c)] <- args
        -> reduce (mkIntCLit te Word64Literal c ty)
      _ -> Nothing

  ]
