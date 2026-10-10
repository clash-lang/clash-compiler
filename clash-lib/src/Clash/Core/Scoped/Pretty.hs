{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Pretty printing of scoped Clash Core, by converting it to Clash Core (see
  "Clash.Core.Scoped.Convert"). The uniques of the printed variables are their
  raw names.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Core.Scoped.Pretty () where

import Clash.Core.Pretty (PrettyPrec (..))

import Clash.Core.Scoped.Convert (toClashTerm, toClashType)
import Clash.Core.Scoped.Syntax (Term, Type)

instance PrettyPrec (Type n) where
  pprPrec p = pprPrec p . toClashType
  pprPrec' opts p = pprPrec' opts p . toClashType

instance PrettyPrec (Term n) where
  pprPrec p = pprPrec p . toClashTerm
  pprPrec' opts p = pprPrec' opts p . toClashTerm
