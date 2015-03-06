-- | Free variable calculations
module CLaSH.Core.FreeVars where

import Unbound.Generics.LocallyNameless (Collection, fv)

import CLaSH.Core.Term         (Term, TmName)
import CLaSH.Core.Type         (TyName, Type)

-- | Gives the free type-variables in a Type
typeFreeVars :: Collection c
             => Type
             -> c TyName
typeFreeVars = fv

-- | Gives the free type-variables and free term-variables of a Term
termFreeVars :: Collection c
             => Term
             -> (c TyName, c TmName)
termFreeVars tm = (termFreeTyVars tm, termFreeIds tm)

-- | Gives the free term-variables of a Term
termFreeIds :: Collection c
            => Term
            -> c TmName
termFreeIds = fv

-- | Gives the free type-variables of a Term
termFreeTyVars :: Collection c
               => Term
               -> c TyName
termFreeTyVars = fv
