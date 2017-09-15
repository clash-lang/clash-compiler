{-|
  Copyright   :  (C) 2012-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Free variable calculations
-}

module Clash.Core.FreeVars where

import Control.Lens.Fold                (Fold)
import Unbound.Generics.LocallyNameless (fv)

import Clash.Core.Term                  (Term, TmOccName)
import Clash.Core.Type                  (TyOccName, Type)

-- | Gives the free type-variables in a Type
typeFreeVars :: Fold Type TyOccName
typeFreeVars = fv

-- | Gives the free term-variables of a Term
termFreeIds :: Fold Term TmOccName
termFreeIds = fv

-- | Gives the free type-variables of a Term
termFreeTyVars :: Fold Term TyOccName
termFreeTyVars = fv
