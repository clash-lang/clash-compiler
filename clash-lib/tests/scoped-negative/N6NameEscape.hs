{-# LANGUAGE GADTs, RankNTypes, DataKinds, ScopedTypeVariables #-}
-- EXPECT: rigid type variable
-- | Escape: use the name of a fresh binder outside of its scope
module N6NameEscape where

import qualified Clash.Core.Name as C

import Clash.Core.Subst
import Clash.Core.Syntax

occ :: C.Name Term
occ = C.mkUnsafeSystemName (error "occ") 0

leak :: Distinct n => Scope n -> Type n -> TmName n
leak sc ty = withFreshIdBinder sc occ ty $ \_ b -> tmNameOf b
