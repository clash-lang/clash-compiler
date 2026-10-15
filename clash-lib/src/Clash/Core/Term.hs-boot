{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-|
  Copyright   :  (C) 2012-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
module Clash.Core.Term where

import Clash.Core.Name (Name)
import GHC.Generics (Generic)

data Term

data TickInfo

type TmName = Name Term

instance Generic Term
