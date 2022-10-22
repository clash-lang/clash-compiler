{-|
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Utility class to extract free variables from data which has variables.
-}

{-# LANGUAGE FlexibleInstances #-}

module Clash.Core.HasFreeVars
  ( HasFreeVars(..)
  ) where

import Control.Lens as Lens (foldMapOf)
import Data.Monoid (All(..), Any(..))

import Clash.Core.FreeVars
import Clash.Core.Term (Term)
import Clash.Core.Type (Type(..))
import Clash.Core.Var (Var)
import Clash.Core.VarSet (VarSet)
import qualified Clash.Core.VarSet as VarSet

class HasFreeVars a where
  {-# MINIMAL freeVarsOf #-}

  freeVarsOf :: a -> VarSet

  {-# INLINE isClosed #-}
  -- | Something is closed if it has no free variables.
  -- This function may be replaced with a more efficient implementation.
  isClosed :: a -> Bool
  isClosed = VarSet.null . freeVarsOf

  {-# INLINE elemFreeVars #-}
  -- | Check if a variable is free in the given value.
  -- This function may be replaced with a more efficient implementation.
  elemFreeVars :: Var a -> a -> Bool
  elemFreeVars v = VarSet.elem v . freeVarsOf

  {-# INLINE notElemFreeVars #-}
  -- | Check if a variable is not free in the given value.
  -- This function may be replaced with a more efficient implementation.
  notElemFreeVars :: Var a -> a -> Bool
  notElemFreeVars x = VarSet.notElem x . freeVarsOf

  {-# INLINE subsetFreeVars #-}
  -- | Check if all variables in a set are free in the given value.
  -- This function may be replaced with a more efficient implementation.
  subsetFreeVars :: VarSet -> a -> Bool
  subsetFreeVars xs = VarSet.subset xs . freeVarsOf

  {-# INLINE disjointFreeVars #-}
  -- | Check if no variables in a set are free in the given value.
  -- This function may be replaced with a more efficient implementation.
  disjointFreeVars :: VarSet -> a -> Bool
  disjointFreeVars xs = VarSet.disjoint xs . freeVarsOf

instance HasFreeVars Term where
  {-# INLINE freeVarsOf #-}
  freeVarsOf =
    Lens.foldMapOf freeLocalVars VarSet.singleton

  elemFreeVars v e =
    getAny (Lens.foldMapOf freeLocalVars (Any . (== v)) e)

  notElemFreeVars v e =
    getAll (Lens.foldMapOf freeLocalVars (All . (/= v)) e)

  disjointFreeVars vs e =
    getAll (Lens.foldMapOf freeLocalVars (All . (`VarSet.notElem` vs)) e)

instance HasFreeVars Type where
  {-# INLINE freeVarsOf #-}
  freeVarsOf =
    Lens.foldMapOf typeFreeVars VarSet.singleton

  isClosed ty =
    case ty of
      VarTy{} -> False
      ForAllTy{} -> getAll (Lens.foldMapOf typeFreeVars (const (All False)) ty)
      AppTy l r -> isClosed l && isClosed r
      _ -> True

  elemFreeVars v ty =
    getAny (Lens.foldMapOf typeFreeVars (Any . (== v)) ty)

  notElemFreeVars v ty =
    getAll (Lens.foldMapOf typeFreeVars (All . (/= v)) ty)

instance (Foldable f, HasFreeVars a) => HasFreeVars (f a) where
  {-# INLINE freeVarsOf #-}
  freeVarsOf = foldMap freeVarsOf
