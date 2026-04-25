{-|
Copyright   :  (C) 2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Blackbox functions for signal portals.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Clash.Primitives.SignalPortal
  ( portalSourceBBF
  , portalSinkBBF
  ) where

import Data.Either (lefts)
import qualified Data.Text.Lazy as LT
import GHC.Stack (HasCallStack)
import Text.Show.Pretty (ppShow)

import Clash.Core.Term (Term)
import Clash.Core.TermLiteral (termToData)
import Clash.Core.Type (Type)
import Clash.Netlist.BlackBox.Types
import Clash.Netlist.Types

portalSourcePrefix :: LT.Text
portalSourcePrefix = "__CLASH_SIGNAL_PORTAL_SOURCE__:"

portalSinkPrefix :: LT.Text
portalSinkPrefix = "__CLASH_SIGNAL_PORTAL_SINK__:"

portalSourceBBF :: HasCallStack => BlackBoxFunction
portalSourceBBF _isD _primName args _resTys =
  case findPortalName args of
    Just portalName ->
      pure $ Right (bbMeta, BBTemplate [Text (portalSourcePrefix <> LT.pack portalName)])
    Nothing ->
      pure $ Left $ "Could not find statically known signal portal name in:\n" <> ppShow args

portalSinkBBF :: HasCallStack => BlackBoxFunction
portalSinkBBF _isD _primName args _resTys =
  case findPortalName args of
    Just portalName ->
      pure $ Right (bbMeta, BBTemplate [Text (portalSinkPrefix <> LT.pack portalName)])
    Nothing ->
      pure $ Left $ "Could not find statically known signal portal name in:\n" <> ppShow args

bbMeta :: BlackBoxMeta
bbMeta = emptyBlackBoxMeta{bbKind = TDecl}

findPortalName :: [Either Term Type] -> Maybe String
findPortalName args =
  case [s | term <- lefts args, Right s <- [termToData @String term]] of
    portalName : _ -> Just portalName
    [] -> Nothing
