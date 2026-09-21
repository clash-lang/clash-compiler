{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
module Clash.GHC.Evaluator.Primitives.Clash.Annotations.BitRepresentation.Deriving
  ( primitives,
  )
where

import qualified Clash.Annotations.BitRepresentation.Deriving
import Clash.Core.Evaluator.Types
import Clash.Core.Term (mkApps)
import Clash.GHC.Evaluator.Primitive.Util
import Clash.Util (textNameLit)
import Data.Text (Text)

primitives :: [(Text, PrimStep)]
primitives =
  [ primStepEntry $(textNameLit 'Clash.Annotations.BitRepresentation.Deriving.dontApplyInHDL) $ \case
      PrimStepContext {..}
        | isSubj,
          f : a : _ <- args ->
            reduceWHNF (mkApps (valToTerm f) [Left (valToTerm a)])
      _ -> Nothing
  ]
