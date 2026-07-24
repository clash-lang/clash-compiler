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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.GHC.Evaluator.Primitives.GHC.Magic
  ( primitives
  ) where

import           Data.Text           (Text)

import           Clash.Core.Evaluator.Types

#if MIN_VERSION_ghc(9,12,0)
import           Clash.Core.DataCon  (DataCon (..))
import           Clash.Core.Literal  (Literal (..))
import Clash.Core.Term (Term (..))
import Clash.Util (textNameLit)

import qualified GHC.Magic
#endif

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
  -- XXX: Does not seem to exist?
  [ ( "GHC.Magic.noinlineConstraint"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [arg] <- args
            -> reduce (valToTerm arg)
          _ -> Nothing
    )
  -- XXX: Does not seem to exist?
  , ( "GHC.Magic.nospec"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [arg] <- args
            -> reduce (valToTerm arg)
          _ -> Nothing
    )
#if MIN_VERSION_ghc(9,12,0)
  , ( $(textNameLit 'GHC.Magic.dataToTag#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [DC dc _] <- args
            -> reduce (Literal (IntLiteral (toInteger (dcTag dc - 1))))
          _ -> Nothing
    )
#endif
  ]
