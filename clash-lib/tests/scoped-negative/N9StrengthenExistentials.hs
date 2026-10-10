{-# LANGUAGE GADTs, RankNTypes, DataKinds, ScopedTypeVariables #-}
-- EXPECT: Couldn't match type ‘TyBinder’ with ‘IdBinder’
-- | Existentials: move a type out of the scope of a pattern as if its
-- existential type variables were term variables (the bug class of
-- `caseCast` and `extractHeadTail`)
module N9StrengthenExistentials where

import Clash.Core.Scoped.Subst
import Clash.Core.Scoped.Syntax

strengthenPat :: Pat n l -> Type l -> Maybe (Type n)
strengthenPat p ty = case p of
  DataPat _ tvs ids -> Just (strengthenTyIds tvs (strengthenTyIds ids ty))
  LitPat _ -> Just ty
  DefaultPat -> Just ty
