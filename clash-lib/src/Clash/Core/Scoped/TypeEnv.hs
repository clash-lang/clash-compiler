{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  The environment of the types of scoped Clash Core: the type constructors
  that types refer to.

  The functions on types that look up type constructors ("Clash.Core.Scoped.Type",
  "Clash.Core.Scoped.HasType", ...) take a t'TypeEnv' where their Clash
  counterparts take a 'TyConMap'. Make one with 'mkTypeEnv'.

  Type constructors, data constructors, global variables and primitives store
  their scoped types and signatures ('tyConKind', 'Clash.Core.TyCon.tyConEqns',
  'Clash.Core.TyCon.ntSig', 'dcType', 'dcSig', 'globalIdType', 'primType'), so
  the lookups below only read them. The ones on data constructors don't use the
  t'TypeEnv' at all: they are here so that their users keep working until they
  read the fields themselves.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Clash.Core.Scoped.TypeEnv
  ( TypeEnv
  , mkTypeEnv
  , typeEnvTyConMap
    -- * Type constructors
  , lookupTyCon
  , findTyCon
  , tyConKindOf
  , newTyConSigOf
  , tyFamEqnsOf
    -- * Data constructors
  , dataConSigOf
  , dataConType
  ) where

import GHC.Stack (HasCallStack)

import Clash.Core.DataCon (DataCon (dcSig, dcType))
import Clash.Core.TyCon (TyCon (tyConKind), TyConMap, TyConName)
import qualified Clash.Data.UniqMap as UniqMap

import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.TyCon

-- | The environment of the types of scoped Clash Core, see the module header
newtype TypeEnv = TypeEnv
  { typeEnvTyConMap :: TyConMap
  -- ^ The type constructors
  }

-- | The environment of the given type constructors
mkTypeEnv :: TyConMap -> TypeEnv
mkTypeEnv = TypeEnv

-- * Type constructors

-- | Look up a type constructor
lookupTyCon :: TypeEnv -> TyConName -> Maybe TyCon
lookupTyCon env tc = UniqMap.lookup tc (typeEnvTyConMap env)

-- | Look up a type constructor. Errors if there is none.
findTyCon :: HasCallStack => TypeEnv -> TyConName -> TyCon
findTyCon env tc = UniqMap.find tc (typeEnvTyConMap env)

-- | The kind of a type constructor, in any scope. Errors if there is no such
-- type constructor.
tyConKindOf :: (HasCallStack, Distinct n) => TypeEnv -> TyConName -> Kind n
tyConKindOf env tc = sinkClosed (tyConKind (findTyCon env tc))

-- | The signature of a newtype, see 'newTyConSig'. 'Nothing' if the type
-- constructor is not a newtype. Errors if there is no such type constructor.
newTyConSigOf :: HasCallStack => TypeEnv -> TyConName -> Maybe NewTyConSig
newTyConSigOf env tc = newTyConSig (findTyCon env tc)

-- | The equations of a type family, see 'tyFamEqns'. 'Nothing' if the type
-- constructor is not a type family, or if there is no such type constructor.
tyFamEqnsOf :: TypeEnv -> TyConName -> Maybe [TyFamEqn]
tyFamEqnsOf env tc = lookupTyCon env tc >>= tyFamEqns

-- * Data constructors

-- | The signature of a data constructor
dataConSigOf :: TypeEnv -> DataCon -> DataConSig
dataConSigOf _ = dcSig

-- | The type of a data constructor, in any scope
dataConType :: Distinct n => TypeEnv -> Scope n -> DataCon -> Type n
dataConType _ _ dc = sinkClosed (dcType dc)
