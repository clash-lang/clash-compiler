{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.GHC.Evaluator.Primitives.Clash.Annotations.BitRepresentation.Deriving
  ( primitives
  ) where

import           Data.Text           (Text)

import           Clash.Core.Evaluator.Types
import Clash.Core.Term (mkApps)
import Clash.Util (textNameLit)

import qualified Clash.Annotations.BitRepresentation.Deriving

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
  [ ( $(textNameLit 'Clash.Annotations.BitRepresentation.Deriving.dontApplyInHDL)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , f : a : _ <- args
            -> reduceWHNF (mkApps (valToTerm f) [Left (valToTerm a)])
          _ -> Nothing
    )
  ]
