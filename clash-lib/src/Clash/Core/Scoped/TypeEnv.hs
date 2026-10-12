{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  The environment of the types of scoped Clash Core: the type constructors
  that types refer to, and the signatures of type constructors and data
  constructors as scoped types.

  The functions on types that look up type constructors ("Clash.Core.Scoped.Type",
  "Clash.Core.Scoped.HasType", ...) take a t'TypeEnv' where their Clash
  counterparts take a 'TyConMap'. Make one per run with 'mkTypeEnv', and pass
  that one around: e.g. the one of 'Clash.Rewrite.Types.scopedTypeEnv'.

  = Transition

  Clash's 'TyCon' stores closed Clash 'C.Type's, which scoped code needs as
  scoped types. A t'TypeEnv' caches their conversion, so that every signature
  is converted at most once per t'TypeEnv': for every type constructor of the
  'TyConMap', its kind, the signature of a newtype ('NewTyConSig') and the
  equations of a type family ('TyFamEqn').

  The cache is built lazily: making a t'TypeEnv' is cheap, and a signature is
  only converted when it is first needed.

  Global variables, primitives and data constructors carry their scoped
  types ('globalIdType', 'primType', 'dcType' and 'dcSig').

  = Migration

  TODO(D4): once 'TyCon' stores scoped types (decision D4 of the
  port), the lookups below read them, and
  a t'TypeEnv' is just the 'TyConMap'. Their users keep working.
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
import Clash.Data.UniqMap (UniqMap)
import qualified Clash.Data.UniqMap as UniqMap

import Clash.Core.Scoped.Convert (fromClashTypeClosed)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.TyCon

-- | The environment of the types of scoped Clash Core, see the module header
data TypeEnv = TypeEnv
  { typeEnvTyConMap :: !TyConMap
  -- ^ The type constructors
  , _teTyCons :: UniqMap TyConInfo
  -- ^ The type constructors, with their converted signatures. Lazy, like the
  -- elements, see the module header.
  }

-- | A type constructor, and its converted signatures
data TyConInfo = TyConInfo
  { tciTyCon :: !TyCon
  , tciKind :: Kind 'VoidS
  , tciNewTyConSig :: Maybe NewTyConSig
  , tciTyFamEqns :: Maybe [TyFamEqn]
  }

-- | The environment of the given type constructors, with their signatures
mkTypeEnv :: TyConMap -> TypeEnv
mkTypeEnv tcm = TypeEnv
  { typeEnvTyConMap = tcm
  , _teTyCons = fmap tyConInfo tcm
  }
 where
  tyConInfo tc = TyConInfo
    { tciTyCon = tc
    , tciKind = fromClashTypeClosed emptyScope (tyConKind tc)
    , tciNewTyConSig = newTyConSig tc
    , tciTyFamEqns = tyFamEqns tc
    }

lookupTyConInfo :: TypeEnv -> TyConName -> Maybe TyConInfo
lookupTyConInfo env tc = UniqMap.lookup tc (_teTyCons env)
{-# INLINE lookupTyConInfo #-}

findTyConInfo :: HasCallStack => TypeEnv -> TyConName -> TyConInfo
findTyConInfo env tc = UniqMap.find tc (_teTyCons env)
{-# INLINE findTyConInfo #-}

-- * Type constructors

-- | Look up a type constructor
lookupTyCon :: TypeEnv -> TyConName -> Maybe TyCon
lookupTyCon env tc = tciTyCon <$> lookupTyConInfo env tc

-- | Look up a type constructor. Errors if there is none.
findTyCon :: HasCallStack => TypeEnv -> TyConName -> TyCon
findTyCon env tc = tciTyCon (findTyConInfo env tc)

-- | The kind of a type constructor, in any scope. Errors if there is no such
-- type constructor.
tyConKindOf :: (HasCallStack, Distinct n) => TypeEnv -> TyConName -> Kind n
tyConKindOf env tc = sinkClosed (tciKind (findTyConInfo env tc))

-- | The signature of a newtype, see 'newTyConSig'. 'Nothing' if the type
-- constructor is not a newtype. Errors if there is no such type constructor.
newTyConSigOf :: HasCallStack => TypeEnv -> TyConName -> Maybe NewTyConSig
newTyConSigOf env tc = tciNewTyConSig (findTyConInfo env tc)

-- | The equations of a type family, see 'tyFamEqns'. 'Nothing' if the type
-- constructor is not a type family, or if there is no such type constructor.
tyFamEqnsOf :: TypeEnv -> TyConName -> Maybe [TyFamEqn]
tyFamEqnsOf env tc = lookupTyConInfo env tc >>= tciTyFamEqns

-- * Data constructors

-- | The signature of a data constructor
dataConSigOf :: TypeEnv -> DataCon -> DataConSig
dataConSigOf _ = dcSig

-- | The type of a data constructor, in any scope
dataConType :: Distinct n => TypeEnv -> Scope n -> DataCon -> Type n
dataConType _ _ dc = sinkClosed (dcType dc)
