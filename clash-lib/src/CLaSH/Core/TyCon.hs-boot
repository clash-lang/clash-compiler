{-|
  Copyright   :  (C) 2012-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

module CLaSH.Core.TyCon where

import Unbound.Generics.LocallyNameless (Name)

data TyCon
type TyConName = Name TyCon
