{-|
  Copyright   :  (C) 2022     , Myrtle.ai,
                     2023     , QBayLogic B.V.,
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Blackbox functions for primitives in the @Clash.Magic@ module.
-}

{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Primitives.Magic
  ( clashCompileErrorBBF
  ) where

import Data.Either (lefts)
import GHC.Stack (HasCallStack)
import Text.Show.Pretty

import Clash.Core.TermLiteral (termToDataError)
import Clash.Netlist.BlackBox.Types (BlackBoxFunction)
import Clash.Netlist.Types ()

clashCompileErrorBBF :: HasCallStack => BlackBoxFunction
clashCompileErrorBBF _isD _primName args _ty
  |   _hasCallstack
    : (either error id . termToDataError -> msg)
    : _ <- lefts args
  = pure $ Left $ "clashCompileError: " <> msg
  | otherwise
  = pure $ Left $ show 'clashCompileErrorBBF <> ": bad args:\n" <> ppShow args
