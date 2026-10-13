{-# LANGUAGE GADTs, RankNTypes, DataKinds, ScopedTypeVariables #-}
-- EXPECT: Could not deduce .*(Distinct|Ext)
-- | Capture: move a term under a binder without going under it with
-- 'openIdBinder', which would rename the binder if it clashes (the bug in
-- `appProp`)
module N4MoveUnderBinder where

import Clash.Core.Syntax

pushUnder :: Term n -> Term n -> Term n
pushUnder (Lam b e) arg = Lam b (App e (sink arg))
pushUnder fun arg = App fun arg
