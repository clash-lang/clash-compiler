{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Temporary 'Binary' instances of closed terms of scoped Clash Core, which
  serializes them as terms of Clash Core ("Clash.Core.Term"), and of global
  variables, which serializes them as their 'C.Id'. E.g. the manifest hashes
  the call graph of a top entity with them
  ('Clash.Driver.Manifest.readFreshManifest'), and the inputs of the profiling
  tools are serialized with them.

  It only uses the conversions of "Clash.Core.Scoped.Convert", and adds
  nothing to the trusted core. Converting to Clash Core uses the raw names of
  the binders as uniques, so equal terms serialize equally.

  TODO(Phase 7): replace it by a native instance in
  "Clash.Core.Scoped.Syntax".
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Core.Scoped.Binary () where

import Data.Binary (Binary (..), Get)

import qualified Clash.Core.Var as C (Id)

import Clash.Core.Scoped.Convert (fromClashTermClosed, mkGlobalIdFromClash, toClashTerm)
import Clash.Core.Scoped.Syntax (GlobalId (globalIdLegacy), S (VoidS), Term)

-- | Through Clash Core, see the module header
instance Binary (Term 'VoidS) where
  put = put . toClashTerm
  get = fromClashTermClosed <$> get

-- | Through Clash Core, see the module header
instance Binary GlobalId where
  put = put . globalIdLegacy
  get = mkGlobalIdFromClash <$> (get :: Get C.Id)
