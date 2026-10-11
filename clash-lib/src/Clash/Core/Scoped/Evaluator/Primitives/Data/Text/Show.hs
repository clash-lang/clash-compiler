{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "Data.Text.Show" in the scoped evaluator:
  ports of @Clash.GHC.Evaluator.Primitives.Data.Text.Show@. See
  "Clash.Core.Scoped.Evaluator.Primitives" for how to port them.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Core.Scoped.Evaluator.Primitives.Data.Text.Show
  ( primitives
  ) where

import qualified Data.Primitive.ByteArray as BA
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Array as Text
import qualified Data.Text.Internal as Text

import Clash.Core.Literal (Literal (..))

import Clash.Core.Scoped.Evaluator.Primitives.Util
import Clash.Core.Scoped.Evaluator.Types
import Clash.Core.Scoped.Syntax

primitives :: [(Text, PrimReduction)]
primitives =
  [ primStepEntry "Data.Text.Show.$wunpackCStringAscii#" $ \case
      PrimStepContext{..}
        | [Lit (StringLiteral addr)] <- args
        , Text.Text (Text.ByteArray ba) _off len <- Text.pack addr
        -> reduce $ mkResultCon te sc ty tys
             [ Literal (ByteArrayLiteral (BA.ByteArray ba))
             , Literal (IntLiteral 0)
             , Literal (IntLiteral (toInteger len)) ]
      _ -> Nothing

  ]
