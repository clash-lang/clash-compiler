{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.GHC.Evaluator.Primitives.Data.Singletons.TypeLits.Internal
  ( primitives
  ) where

import           Data.Text           (Text)

import           Clash.Core.Evaluator.Types
import           Clash.Core.Literal  (Literal (..))
import Clash.Core.Term (Term (..))

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
  -- Type level ^    -- XXX: Very fragile
  -- These is are specialized versions of ^_f, named by some combination of ghc and singletons.
  [ ( "Data.Singletons.TypeLits.Internal.$fSingI->^@#@$_f"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- ghc-8.6.5, singletons-2.5.1
            | [i,j] <- naturalLiterals' args
            -> reduce (Literal (NaturalLiteral (i ^ j)))
          _ -> Nothing
    )
  , ( "Data.Singletons.TypeLits.Internal.%^_f"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}             -- ghc-8.8.1, singletons-2.6
            | [i,j] <- naturalLiterals' args
            -> reduce (Literal (NaturalLiteral (i ^ j)))
          _ -> Nothing
    )
  ]
