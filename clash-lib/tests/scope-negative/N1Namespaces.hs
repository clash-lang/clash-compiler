{-# LANGUAGE GADTs, RankNTypes, DataKinds, ScopedTypeVariables #-}
-- EXPECT: TmName [a-z0-9]* with actual type: TyName|Couldn't match type ‘TyName’ with ‘TmName’
-- | Namespaces: refer to a type variable as if it were a term variable
module N1Namespaces where

import qualified Clash.Core.Name as C

import Clash.Core.Syntax

occ :: C.Name Term
occ = C.mkUnsafeSystemName (error "occ") 0

tyVarAsTermVar :: TyBinder n l -> Type l -> Term l
tyVarAsTermVar b ty = Var occ ty (tyNameOf b)
