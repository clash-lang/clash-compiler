{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
module Clash.GHC.Evaluator.Primitives.GHC.PrimopWrappers
  ( primitives,
  )
where

import Clash.Core.Evaluator.Types
import Data.Text (Text)
#if MIN_VERSION_ghc_prim(0,12,0)
import Clash.Core.DataCon (DataCon (..))
import Clash.Core.Literal (Literal (..))
import Clash.Core.Term (Term (..))
import Clash.GHC.Evaluator.Primitive.Util
import Clash.Util (textNameLit)
import qualified GHC.PrimopWrappers
#endif

primitives :: [(Text, PrimStep)]
#if MIN_VERSION_ghc_prim(0,12,0)
primitives =
  [ primStepEntry $(textNameLit 'GHC.PrimopWrappers.dataToTagSmall#) $ \case
      PrimStepContext {..}
        | [DC dc _] <- args ->
            reduce (Literal (IntLiteral (toInteger (dcTag dc - 1))))
      _ -> Nothing,
    primStepEntry $(textNameLit 'GHC.PrimopWrappers.dataToTagLarge#) $ \case
      PrimStepContext {..}
        | [DC dc _] <- args ->
            reduce (Literal (IntLiteral (toInteger (dcTag dc - 1))))
      _ -> Nothing
  ]
#else
primitives = []
#endif
