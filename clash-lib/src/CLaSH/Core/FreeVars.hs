-- | Free variable calculations
module CLaSH.Core.FreeVars where

import Control.Lens.Fold                (Fold)
import Unbound.Generics.LocallyNameless (fv)

import CLaSH.Core.Term         (Term, TmName)
import CLaSH.Core.Type         (TyName, Type)

-- | Gives the free type-variables in a Type
typeFreeVars :: Fold Type TyName
typeFreeVars = fv

-- | Gives the free term-variables of a Term
termFreeIds :: Fold Term TmName
termFreeIds = fv

-- | Gives the free type-variables of a Term
termFreeTyVars :: Fold Term TyName
termFreeTyVars = fv
