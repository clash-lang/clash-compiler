{-# LANGUAGE GADTs, RankNTypes, DataKinds, ScopedTypeVariables #-}
-- EXPECT: rigid type variable
-- | Capture: move the arguments of an application under the binders of a
-- let-expression in its function part without opening them (case 3 of Note
-- [AppProp no shadowing], the bug in `appProp`)
module N11AppPropLet where

import Clash.Core.Scoped.Syntax

appPropLet :: Term n -> [Term n] -> Term n
appPropLet fun args = case fun of
  Let bind e -> Let bind (appPropLet e args)
  _ -> foldl App fun args
