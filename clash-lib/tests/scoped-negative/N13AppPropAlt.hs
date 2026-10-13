{-# LANGUAGE GADTs, RankNTypes, DataKinds, ScopedTypeVariables #-}
-- EXPECT: rigid type variable
-- | Capture: push the arguments of an application into the alternatives of
-- a case-expression in its function part without opening their patterns
-- (the capture bug in `appProp` that #1040 fixed)
module N13AppPropAlt where

import Clash.Core.Scoped.Syntax

appPropAlt :: Term n -> [Term n] -> Term n
appPropAlt fun args = case fun of
  Case scrut ty alts -> Case scrut ty [Alt p (foldl App e args) | Alt p e <- alts]
  _ -> foldl App fun args
