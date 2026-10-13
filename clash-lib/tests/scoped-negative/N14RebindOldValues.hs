{-# LANGUAGE GADTs, RankNTypes, DataKinds, ScopedTypeVariables #-}
-- EXPECT: rigid type variable
-- | Rebinding: rebuild an opened let-expression with some of its binders
-- ('rebindIds'), but with its right-hand sides and body as they were, in the
-- scope of all of the binders, rather than the ones 'rebindIds' gives back
-- after checking that the left-out binders don't occur in them
module N14RebindOldValues where

import qualified Clash.Core.Name as C

import Clash.Core.Subst
import Clash.Core.Syntax

rebindLet
  :: (Distinct n, Distinct l)
  => Scope n -> Tele IdBinder n l -> [(C.Name Term, Type n, TmName l)] -> [Term l] -> Term l
  -> Maybe (Term n)
rebindLet sc bs kept xs body = rebindIds sc bs kept (body : xs) $ \_ bs' _ -> Let (Rec bs' xs) body
