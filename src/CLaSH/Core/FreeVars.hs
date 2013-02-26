module CLaSH.Core.FreeVars where

import Unbound.LocallyNameless (fv,Collection)

import CLaSH.Core.Term (Term,TmName)
import CLaSH.Core.Type (Type,TyName)

typeFreeVars ::
  Collection c
  => Type
  -> c TyName
typeFreeVars = fv

termFreeVars ::
  Collection c
  => Term
  -> (c TyName, c TmName)
termFreeVars tm = (termFreeTyVars tm, termFreeIds tm)

termFreeIds ::
  Collection c
  => Term
  -> c TmName
termFreeIds = fv

termFreeTyVars ::
  Collection c
  => Term
  -> c TyName
termFreeTyVars = fv
