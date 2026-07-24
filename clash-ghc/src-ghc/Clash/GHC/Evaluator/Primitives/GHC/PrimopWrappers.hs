{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.GHC.Evaluator.Primitives.GHC.PrimopWrappers
  ( primitives
  ) where

import           Data.Text           (Text)

import           Clash.Core.Evaluator.Types

#if MIN_VERSION_ghc_prim(0,12,0)
import           Clash.Core.DataCon  (DataCon (..))
import           Clash.Core.Literal  (Literal (..))
import Clash.Core.Term (Term (..))
import Clash.Util (textNameLit)

import qualified GHC.PrimopWrappers

import Clash.GHC.Evaluator.Primitive.Util
#endif

primitives :: [(Text, PrimStep)]
#if MIN_VERSION_ghc_prim(0,12,0)
primitives =
  [ ( $(textNameLit 'GHC.PrimopWrappers.dataToTagSmall#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [DC dc _] <- args
            -> reduce (Literal (IntLiteral (toInteger (dcTag dc - 1))))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.PrimopWrappers.dataToTagLarge#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [DC dc _] <- args
            -> reduce (Literal (IntLiteral (toInteger (dcTag dc - 1))))
          _ -> Nothing
    )
  ]
#else
primitives = []
#endif
