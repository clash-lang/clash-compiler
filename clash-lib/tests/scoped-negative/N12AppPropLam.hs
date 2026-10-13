{-# LANGUAGE GADTs, RankNTypes, DataKinds, ScopedTypeVariables #-}
-- EXPECT: rigid type variable
-- | Capture: let-bind the argument of a lambda with the lambda's own binder,
-- keeping the other arguments as they are (the capture bug in `appProp`
-- that #1040 fixed)
module N12AppPropLam where

import Clash.Core.Scoped.Syntax

appPropLam :: Term n -> [Term n] -> Term n
appPropLam fun args = case (fun, args) of
  (Lam b e, arg : rest) -> Let (NonRec b arg) (foldl App e rest)
  _ -> foldl App fun args
