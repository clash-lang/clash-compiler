{-# LANGUAGE GADTs, RankNTypes, DataKinds, ScopedTypeVariables #-}
-- EXPECT: TmNameBinder [a-z0-9]* [a-z0-9]* with actual type: TyNameBinder|Couldn't match type ‘TyNameBinder’ with ‘TmNameBinder’
-- | Namespaces: bind a term variable with the binder of a type variable, which
-- would let 'strengthenTyId' move the type variable out of its scope
module N7Transplant where

import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C

import Clash.Core.Scoped.Syntax

occ :: C.Name C.Term
occ = C.mkUnsafeSystemName (error "occ") 0

transplant :: TyBinder n l -> IdBinder n l
transplant (TyBinder _ k nb) = IdBinder occ k nb
