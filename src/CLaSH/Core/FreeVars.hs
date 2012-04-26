module CLaSH.Core.FreeVars where

import Unbound.LocallyNameless (fv)

import CLaSH.Core.Term (Term,TmName)
import CLaSH.Core.Type (Type,TyName)

typeFreeVars ::
  Type
  -> [TyName]
typeFreeVars = fv

termFreeVars ::
  Term
  -> ([TyName],[TmName])
termFreeVars tm = (termFreeTyVars tm, termFreeIds tm)

termFreeIds ::
  Term
  -> [TmName]
termFreeIds = fv

termFreeTyVars ::
  Term
  -> [TyName]
termFreeTyVars = fv
