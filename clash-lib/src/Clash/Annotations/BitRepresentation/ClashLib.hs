{-|
Copyright  :  (C) 2018, Google Inc.
                  2022, LUMI GUIDE FIETSDETECTIE B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

Houses internal BitRepresentation code which cannot be housed in clash-prelude
due to its dependencies.
-}

{-# LANGUAGE TemplateHaskell #-}

module Clash.Annotations.BitRepresentation.ClashLib
  ( bitsToBits
  ) where

import qualified Clash.Annotations.BitRepresentation.Util as BitRepresentation
import qualified Clash.Netlist.Types                      as Netlist

bitToBit
  :: BitRepresentation.Bit
  -> Netlist.Bit
bitToBit BitRepresentation.H = Netlist.H
bitToBit BitRepresentation.L = Netlist.L
bitToBit BitRepresentation.U = Netlist.U

-- | Converts a list of /BitRepresentation.Bit/s to their Netlist counterpart.
bitsToBits
  :: [BitRepresentation.Bit]
  -> [Netlist.Bit]
bitsToBits = map bitToBit
