{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Type constructors in scoped Clash Core: the right-hand sides of newtypes and
  the equations of type families, as scoped types.

  = Transition

  Scoped types refer to type constructors by name (in a 'C.ConstTy'), and
  look them up in Clash's 'TyConMap', whose 'TyCon's have closed Clash
  'C.Type's. Like "Clash.Core.Scoped.DataCon" does for data constructors, this
  module gives the parts of a 'TyCon' that mention type variables as scoped
  types, by converting them:

  * The /signature of a newtype/ (t'NewTyConSig'): a telescope of the type
    variables of the newtype, in the empty scope, and its eta-reduced
    representation type in their scope. Unwrapping a newtype is then
    substituting types for these type variables ('newTyConInstRhs').

  * The /equations of a type family/ (t'TyFamEqn'): for every equation, a
    telescope of its pattern variables, in the empty scope, and its left-hand
    side patterns and right-hand side in their scope. Clash stores the
    equations without their pattern variables, so these are the free type
    variables of the equation, ordered so that every variable comes after the
    variables in its kind. Reducing a type family application is then matching
    the patterns and substituting the matched types for the pattern variables
    (see 'Clash.Core.Scoped.Type.reduceTypeFamily').

  The kind of a type constructor is closed, and converted into any scope by
  'tyConKindOf'.

  The conversion is redone for every use. It is cheap, as these types are
  small, and type family equations are only converted when they are tried; if
  it shows up in profiles, cache the signatures by 'tyConUniq'.

  = Migration

  Once 'TyCon' stores scoped types (in place of 'ntEtadRhs' and 'tyConSubst'),
  'newTyConSig' and 'tyFamEqns' become field selectors, and their users keep
  working.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Core.Scoped.TyCon
  ( -- * Newtypes
    NewTyConSig (..)
  , newTyConSig
  , newTyConInstRhs
    -- * Type families
  , TyFamEqn (..)
  , tyFamEqns
    -- * Kinds
  , tyConKindOf
  ) where

import qualified Control.Lens as Lens
import qualified Data.List as List
import GHC.Stack (HasCallStack)

import Clash.Core.FreeVars (dependencySortTyVars, typeFreeVars)
import Clash.Core.TyCon (AlgTyConRhs (..), TyCon (..))
import qualified Clash.Core.Type as C
import qualified Clash.Core.Var as C

import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.Subst
import Clash.Core.Scoped.Syntax

-- * Newtypes

-- | The signature of a newtype, see the module header
data NewTyConSig where
  NewTyConSig
    :: DExt 'VoidS p
    => Tele TyBinder 'VoidS p
    -- ^ The type variables of the newtype
    -> Type p
    -- ^ Its representation type, eta-reduced
    -> NewTyConSig

deriving instance Show NewTyConSig

-- | The signature of a newtype. 'Nothing' if the type constructor is not a
-- newtype. Errors if the representation type mentions type variables other
-- than those of the newtype.
newTyConSig :: HasCallStack => TyCon -> Maybe NewTyConSig
newTyConSig = \case
  AlgTyCon {algTcRhs = NewTyCon {ntEtadRhs = (tvs, rhs)}} ->
    Just $ fromClashTyVars emptyConvEnv tvs $ \env tele ->
      NewTyConSig tele (fromClashType env rhs)
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

-- | An equation of a type family, see the module header
data TyFamEqn where
  TyFamEqn
    :: DExt 'VoidS p
    => Tele TyBinder 'VoidS p
    -- ^ The pattern variables
    -> [Type p]
    -- ^ The patterns of the left-hand side
    -> Type p
    -- ^ The right-hand side
    -> TyFamEqn

deriving instance Show TyFamEqn

-- | The equations of a type family, in order. 'Nothing' if the type
-- constructor is not a type family.
tyFamEqns :: HasCallStack => TyCon -> Maybe [TyFamEqn]
tyFamEqns = \case
  FunTyCon {tyConSubst = eqns} -> Just (map tyFamEqn eqns)
  _ -> Nothing

tyFamEqn :: HasCallStack => ([C.Type], C.Type) -> TyFamEqn
tyFamEqn (lhs, rhs) =
  fromClashTyVars emptyConvEnv tvs $ \env tele ->
    TyFamEqn tele (map (fromClashType env) lhs) (fromClashType env rhs)
 where
  -- The free type variables, closed over kinds, see the module header
  tvs =
    dependencySortTyVars
      (List.nubBy sameUniq (concatMap (Lens.toListOf typeFreeVars) (lhs ++ [rhs])))

  sameUniq :: C.TyVar -> C.TyVar -> Bool
  sameUniq v1 v2 = C.varUniq v1 == C.varUniq v2

-- * Kinds

-- | The kind of a type constructor, in any scope
tyConKindOf :: (HasCallStack, Distinct n) => Scope n -> TyCon -> Kind n
tyConKindOf sc tc = fromClashTypeClosed sc (tyConKind tc)
