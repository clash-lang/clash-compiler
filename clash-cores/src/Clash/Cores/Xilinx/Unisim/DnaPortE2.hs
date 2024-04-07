{-|
Copyright   :  (C) 2022-2024, Google Inc.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Primitive and utility functions for reading the DNA port of Xilinx FPGAs.
-}
module Clash.Cores.Xilinx.Unisim.DnaPortE2
  ( readDnaPortE2
  , simDna2
  , isValidDna
  , dnaPortE2
  ) where

import Clash.Cores.Xilinx.Unisim.DnaPortE2.Internal
