{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  The signatures of type constructors in scoped Clash Core
  ("Clash.Core.Scoped.Sig"): the signature of a newtype ('newTyConSig') and the
  equations of a type family ('tyFamEqns'), and the instantiation of a newtype
  ('newTyConInstRhs').
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Core.Scoped.TyCon
  ( -- * Newtypes
    NewTyConSig (..)
  , newTyConSig
  , newTyConInstRhs
    -- * Type families
  , TyFamEqn (..)
  , tyFamEqns
    -- * Data constructors
  , DataConSig (..)
  ) where

import qualified Data.List as List

import Clash.Core.TyCon (AlgTyConRhs (..), TyCon (..))

import Clash.Core.Scoped.Sig
import Clash.Core.Scoped.Subst
import Clash.Core.Scoped.Syntax

-- * Newtypes

-- | The signature of a newtype. 'Nothing' if the type constructor is not a
-- newtype.
newTyConSig :: TyCon -> Maybe NewTyConSig
newTyConSig = \case
  AlgTyCon {algTcRhs = NewTyCon {ntSig}} -> Just ntSig
  _ -> Nothing

-- | The representation type of a newtype applied to the given types: the types
-- are substituted for the type variables of the newtype, and the remaining
-- ones are applied to the result. 'Nothing' if there are fewer types than
-- type variables.
--
-- See 'Clash.Core.Type.coreView1'.
newTyConInstRhs :: Distinct n => Scope n -> NewTyConSig -> [Type n] -> Maybe (Type n)
newTyConInstRhs sc (NewTyConSig tvs rhs) = instTele (closedSubst sc) tvs rhs

-- Substitute the types for the variables of the telescope, and apply the
-- remaining ones
instTele
  :: Distinct n => Subst i n -> Tele TyBinder i p -> Type p -> [Type n] -> Maybe (Type n)
instTele s TNil rhs tys = Just (List.foldl' AppTy (substTy s rhs) tys)
instTele s (TCons b bs) rhs (ty:tys) = instTele (extendTySubst s b ty) bs rhs tys
instTele _ TCons {} _ [] = Nothing

-- * Type families

-- | The equations of a type family, in order. 'Nothing' if the type
-- constructor is not a type family.
tyFamEqns :: TyCon -> Maybe [TyFamEqn]
tyFamEqns = \case
  FunTyCon {tyConEqns} -> Just tyConEqns
  _ -> Nothing
