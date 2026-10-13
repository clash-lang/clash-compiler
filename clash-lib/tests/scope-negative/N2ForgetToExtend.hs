{-# LANGUAGE GADTs, RankNTypes, DataKinds, ScopedTypeVariables #-}
-- EXPECT: rigid type variable
-- | Freshness: create a binder, but keep using the old scope
module N2ForgetToExtend where

import qualified Clash.Core.Name as C

import Clash.Core.Subst
import Clash.Core.Syntax

occ :: C.Name Term
occ = C.mkUnsafeSystemName (error "occ") 0

forgetToExtend :: Distinct n => Scope n -> Type n -> Term n -> Term n
forgetToExtend sc ty e =
  withFreshIdBinder sc occ ty $ \_sc1 b ->
    Let (NonRec b e) (useScope sc (idBinderVar b))
 where
  useScope :: Scope m -> Term m -> Term m
  useScope _ t = t
