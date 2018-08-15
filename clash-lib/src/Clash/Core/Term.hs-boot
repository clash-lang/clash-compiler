{-|
  Copyright   :  (C) 2012-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Clash.Core.Term where

import GHC.Generics    (Generic)
import Clash.Core.Name (Name)

data Term

type TmName = Name Term

instance Generic Term
