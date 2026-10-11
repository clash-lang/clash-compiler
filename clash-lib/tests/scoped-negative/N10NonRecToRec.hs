{-# LANGUAGE GADTs, RankNTypes, DataKinds, ScopedTypeVariables #-}
-- EXPECT: rigid type variable
-- | NonRec to Rec: reuse the right-hand side of a non-recursive let-binding
-- as the right-hand side of a recursive one, which brings the binder into
-- scope of its own right-hand side. When the binder shadows a variable that
-- the right-hand side refers to, as in @\x -> let x = g x in x@, this makes
-- the loop @letrec x = g x@ (the bug in `allR`, `splitCastWork`, `flattenLet`
-- and `topLet`).
module N10NonRecToRec where

import Clash.Core.Scoped.Syntax

nonRecToRec :: Term n -> Term n
nonRecToRec (Let (NonRec b rhs) body) = Let (Rec (TCons b TNil) [rhs]) body
nonRecToRec e = e
