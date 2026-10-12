{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  The signatures of type constructors and data constructors in scoped Clash
  Core: the parts of a 'Clash.Core.TyCon.TyCon' and a
  'Clash.Core.DataCon.DataCon' that bind type variables.

  * The /signature of a newtype/ (t'NewTyConSig'): a telescope of the type
    variables of the newtype, in the empty scope, and its eta-reduced
    representation type in their scope. Unwrapping a newtype is then
    substituting types for these type variables
    ('Clash.Core.Scoped.TyCon.newTyConInstRhs').

  * The /equations of a type family/ (t'TyFamEqn'): for every equation, a
    telescope of its pattern variables, in the empty scope, and its left-hand
    side patterns and right-hand side in their scope. Reducing a type family
    application is then matching the patterns and substituting the matched
    types for the pattern variables (see
    'Clash.Core.Scoped.Type.reduceTypeFamily').

  * The /signature of a data constructor/ (t'DataConSig'): a telescope of its
    universally quantified type variables, in the empty scope; a telescope of
    its existentially quantified type variables, in the scope of the universal
    ones; and the types of its fields, in the scope of both. Instantiating a
    data constructor is then substituting types for the variables of these
    telescopes (see "Clash.Core.Scoped.DataCon").

  This module only depends on "Clash.Core.Scoped.Syntax", so that the modules
  of type constructors and data constructors can store the signatures.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Clash.Core.Scoped.Sig
  ( NewTyConSig (..)
  , TyFamEqn (..)
  , DataConSig (..)
  ) where

import Clash.Core.Scoped.Syntax

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

-- | The signature of a data constructor, see the module header
data DataConSig where
  DataConSig
    :: (DExt 'VoidS u, DExt u e)
    => Tele TyBinder 'VoidS u
    -- ^ Universally quantified type variables
    -> Tele TyBinder u e
    -- ^ Existentially quantified type variables
    -> [Type e]
    -- ^ Types of the fields
    -> DataConSig

deriving instance Show DataConSig
