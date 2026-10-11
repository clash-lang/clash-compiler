{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of
  "Clash.Annotations.BitRepresentation.Deriving" in the scoped evaluator:
  ports of
  @Clash.GHC.Evaluator.Primitives.Clash.Annotations.BitRepresentation.Deriving@.
  See "Clash.Core.Scoped.Evaluator.Primitives" for how to port them.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Scoped.Evaluator.Primitives.Clash.Annotations.BitRepresentation.Deriving
  ( primitives
  ) where

import Data.Text (Text)

import Clash.Util (textNameLit)

import qualified Clash.Annotations.BitRepresentation.Deriving

import Clash.Core.Scoped.Evaluator.Primitives.Util
import Clash.Core.Scoped.Evaluator.Types
import Clash.Core.Scoped.Term (mkApps)

primitives :: [(Text, PrimReduction)]
primitives =
  [ primStepEntry $(textNameLit 'Clash.Annotations.BitRepresentation.Deriving.dontApplyInHDL) $ \case
      PrimStepContext{..}
        | isSubj
        , f : a : _ <- args
        -> reduceWHNF (mkApps (valToTerm f) [Left (valToTerm a)])
      _ -> Nothing

  ]
