{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "Data.Singletons.TypeLits.Internal" in the
  scoped evaluator. See "Clash.Core.Evaluator.Primitives" for how to
  write them.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Core.Evaluator.Primitives.Data.Singletons.TypeLits.Internal
  ( primitives
  ) where

import Data.Text (Text)

import Clash.Core.Literal (Literal (..))

import Clash.Core.Evaluator.Primitives.Util
import Clash.Core.Evaluator.Types
import Clash.Core.Syntax

primitives :: [(Text, PrimReduction)]
primitives =
  -- Type level ^    -- XXX: Very fragile
  -- These is are specialized versions of ^_f, named by some combination of ghc and singletons.
  [ primStepEntry "Data.Singletons.TypeLits.Internal.$fSingI->^@#@$_f" $ \case
      PrimStepContext{..} -- ghc-8.6.5, singletons-2.5.1
        | [i,j] <- naturalLiterals' args
        -> reduce (Literal (NaturalLiteral (i ^ j)))
      _ -> Nothing

  , primStepEntry "Data.Singletons.TypeLits.Internal.%^_f" $ \case
      PrimStepContext{..}             -- ghc-8.8.1, singletons-2.6
        | [i,j] <- naturalLiterals' args
        -> reduce (Literal (NaturalLiteral (i ^ j)))
      _ -> Nothing

  ]
