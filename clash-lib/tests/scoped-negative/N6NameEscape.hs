{-# LANGUAGE GADTs, RankNTypes, DataKinds, ScopedTypeVariables #-}
-- EXPECT: rigid type variable
-- | Escape: use the name of a fresh binder outside of its scope
module N6NameEscape where

import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C

import Clash.Core.Scoped.Subst
import Clash.Core.Scoped.Syntax

occ :: C.Name C.Term
occ = C.mkUnsafeSystemName (error "occ") 0

leak :: Distinct n => Scope n -> Type n -> TmName n
leak sc ty = withFreshIdBinder sc occ ty $ \_ b -> tmNameOf b
