{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  A temporary 'Binary' instance of closed terms of scoped Clash Core, which
  serializes them as terms of Clash Core ("Clash.Core.Term"). E.g. the
  manifest hashes the call graph of a top entity with it
  ('Clash.Driver.Manifest.readFreshManifest').

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

import Data.Binary (Binary (..))

import Clash.Core.Scoped.Convert (fromClashTermClosed, toClashTerm)
import Clash.Core.Scoped.Syntax (S (VoidS), Term)

-- | Through Clash Core, see the module header
instance Binary (Term 'VoidS) where
  put = put . toClashTerm
  get = fromClashTermClosed <$> get
