{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  The environment of the types of scoped Clash Core: the type constructors
  that types refer to, and the signatures of type constructors, data
  constructors and global variables as scoped types.

  The functions on types that look up type constructors ("Clash.Core.Scoped.Type",
  "Clash.Core.Scoped.HasType", ...) take a t'TypeEnv' where their Clash
  counterparts take a 'TyConMap'. Make one per run with 'mkTypeEnv', and pass
  that one around: e.g. the one of 'Clash.Rewrite.Types.scopedTypeEnv'.

  = Transition

  Clash's 'TyCon', 'DataCon' and 'C.Id' store closed Clash 'C.Type's, which
  scoped code needs as scoped types. A t'TypeEnv' caches their conversion, so
  that every signature is converted at most once per t'TypeEnv':

  * for every type constructor of the 'TyConMap': its kind, the signature of a
    newtype ('NewTyConSig') and the equations of a type family ('TyFamEqn');

  * for every data constructor of a type constructor of the 'TyConMap': its
    signature ('DataConSig') and its type;

  * for every global variable given to 'addGlobalTypes', such as those that
    have a binding at the start of normalization: its type. The types of
    other global variables are converted for every use.

    A global variable is looked up by its unique, but an occurrence of a
    global variable need not have the type of its binding: e.g. normalization
    reduces the type families in the type of a binding, but not in those of
    its occurrences. So 'globalIdType' only uses the cached type if the Clash
    type of the occurrence is alpha-equivalent to the one it was converted
    from, which is cheaper than converting it. Otherwise it converts the type
    of the occurrence.

  The caches are built lazily: making a t'TypeEnv' is cheap, and a signature
  is only converted when it is first needed.

  The types of primitives are not cached: 'C.PrimInfo' has no unique, and
  primitives with the same name can have different types, e.g. coercions.
  See 'Clash.Core.Scoped.HasType.primInfoType'.

  = Migration

  TODO(D4): once 'TyCon' and 'DataCon' store scoped types (decision D4 of the
  port), and global variables carry theirs, the lookups below read them, and
  a t'TypeEnv' is just the 'TyConMap'. Their users keep working.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Clash.Core.Scoped.TypeEnv
  ( TypeEnv
  , mkTypeEnv
  , addGlobalTypes
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
    -- * Global variables
  , globalIdType
  ) where

import GHC.Stack (HasCallStack)

import Clash.Core.DataCon (DataCon (dcType))
import Clash.Core.Subst (aeqType)
import Clash.Core.TyCon (TyCon (tyConKind), TyConMap, TyConName, tyConDataCons)
import qualified Clash.Core.Type as C
import qualified Clash.Core.Var as C
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
  , _teDataCons :: UniqMap DataConInfo
  -- ^ The data constructors of the type constructors, with their converted
  -- signatures. Lazy, like the elements.
  , _teGlobals :: UniqMap GlobalInfo
  -- ^ The global variables given to 'addGlobalTypes', with their converted
  -- types. The elements are lazy.
  }

-- | A type constructor, and its converted signatures
data TyConInfo = TyConInfo
  { tciTyCon :: !TyCon
  , tciKind :: Kind 'VoidS
  , tciNewTyConSig :: Maybe NewTyConSig
  , tciTyFamEqns :: Maybe [TyFamEqn]
  }

-- | The converted signature and type of a data constructor
data DataConInfo = DataConInfo
  { dciSig :: DataConSig
  , dciType :: Type 'VoidS
  }

-- | The type of a global variable, and that type converted
data GlobalInfo = GlobalInfo
  { _giClashType :: !C.Type
  , giType :: Type 'VoidS
  }

-- | The environment of the given type constructors, with the signatures of
-- them and of their data constructors, but no types of global variables. See
-- 'addGlobalTypes'.
mkTypeEnv :: TyConMap -> TypeEnv
mkTypeEnv tcm = TypeEnv
  { typeEnvTyConMap = tcm
  , _teTyCons = fmap tyConInfo tcm
  , _teDataCons =
      UniqMap.fromList
        [ (dc, dataConInfo dc) | tc <- UniqMap.elems tcm, dc <- tyConDataCons tc ]
  , _teGlobals = UniqMap.empty
  }
 where
  tyConInfo tc = TyConInfo
    { tciTyCon = tc
    , tciKind = fromClashTypeClosed emptyScope (tyConKind tc)
    , tciNewTyConSig = newTyConSig tc
    , tciTyFamEqns = tyFamEqns tc
    }

-- | Cache the types of the given global variables, keyed by their uniques: e.g.
-- the variables of the bindings at the start of normalization. Replaces the
-- types of global variables cached before.
addGlobalTypes :: UniqMap C.Id -> TypeEnv -> TypeEnv
addGlobalTypes gs env = env {_teGlobals = fmap globalInfo gs}
 where
  globalInfo v = GlobalInfo
    { _giClashType = C.varType v
    , giType = fromClashTypeClosed emptyScope (C.varType v)
    }

dataConInfo :: HasCallStack => DataCon -> DataConInfo
dataConInfo dc = DataConInfo
  { dciSig = dataConSig dc
  , dciType = fromClashTypeClosed emptyScope (dcType dc)
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

-- | The signature of a data constructor, see 'dataConSig'
dataConSigOf :: HasCallStack => TypeEnv -> DataCon -> DataConSig
dataConSigOf env dc = case UniqMap.lookup dc (_teDataCons env) of
  Just dci -> dciSig dci
  Nothing -> dataConSig dc

-- | The type of a data constructor, in any scope
dataConType :: (HasCallStack, Distinct n) => TypeEnv -> Scope n -> DataCon -> Type n
dataConType env sc dc = case UniqMap.lookup dc (_teDataCons env) of
  Just dci -> sinkClosed (dciType dci)
  Nothing -> fromClashTypeClosed sc (dcType dc)

-- * Global variables

-- | The type of a global variable, in any scope. See the module header for
-- when it is cached.
globalIdType :: (HasCallStack, Distinct n) => TypeEnv -> Scope n -> GlobalId -> Type n
globalIdType env sc g = case UniqMap.lookup (globalIdUniq g) (_teGlobals env) of
  Just (GlobalInfo cty ty) | aeqType cty gty -> sinkClosed ty
  _ -> fromClashTypeClosed sc gty
 where
  gty = C.varType (globalIdVar g)
