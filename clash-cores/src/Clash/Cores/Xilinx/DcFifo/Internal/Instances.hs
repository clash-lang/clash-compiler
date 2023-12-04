{-|
  Copyright   :  (C) 2022 Google Inc
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Orphan instances for types in "Clash.Cores.Xilinx.DcFifo.Internal.Types". They
  are housed here due to TemplateHaskell stage restrictions.
-}

{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_HADDOCK hide #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Clash.Cores.Xilinx.DcFifo.Internal.Instances where

import Clash.Core.TermLiteral (TermLiteral(..))
import Clash.Core.TermLiteral.TH (deriveTermLiteral)

import Clash.Cores.Xilinx.DcFifo.Internal.Types

deriveTermLiteral ''DcConfig
