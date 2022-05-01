{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Clash.Cores.Xilinx.DcFifo.Instances where

import Clash.Core.TermLiteral (TermLiteral (termToData))
import Clash.Core.TermLiteral.TH (deriveTermToData)

import Clash.Cores.Xilinx.DcFifo.Explicit

-- XXX: We might want to move these in their expanded form to
-- `Clash.Cores.Xilinx.DcFifo.Explicit`.

instance TermLiteral ReadMode where
  termToData = $(deriveTermToData ''ReadMode)

instance TermLiteral DcImplementation where
  termToData = $(deriveTermToData ''DcImplementation)

instance TermLiteral a => TermLiteral (DcConfig a) where
  termToData = $(deriveTermToData ''DcConfig)
