{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  For "Clash.Core.Syntax", whose terms refer to data constructors,
  which store scoped types.
-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Clash.Core.DataCon where

import Control.DeepSeq (NFData)

import Clash.Core.Name (Name)

data DataCon
instance Show DataCon
instance NFData DataCon

type DcName = Name DataCon
type ConTag = Int
data DcStrictness
