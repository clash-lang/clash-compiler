{-|
  Copyright   :  (C) 2022 Google Inc
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Orphan instances for types in 'Clash.Cores.Xilinx.DcFifo.Explicit'. They are
  housed here due to TemplateHaskell stage restrictions.
-}

{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Clash.Cores.Xilinx.DcFifo.Instances where

import Clash.Core.TermLiteral (TermLiteral (termToData))
import Clash.Core.TermLiteral.TH (deriveTermToData)

import Clash.Cores.Xilinx.DcFifo.Explicit

-- XXX: We might want to move these in their expanded form to
-- `Clash.Cores.Xilinx.DcFifo.Explicit`.

instance TermLiteral a => TermLiteral (DcConfig a) where
  termToData = $(deriveTermToData ''DcConfig)
