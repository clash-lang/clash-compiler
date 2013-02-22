module CLaSH.Core.TysPrim where

-- External Modules
import Unbound.LocallyNameless         (makeName)

-- GHC API
import qualified PrelNames
import qualified CLaSH.GHC.Compat.PrelNames as CPrelNames (tySuperKindTyConKey)
import Unique    (getKey)

import CLaSH.Core.TyCon
import {-# SOURCE #-} CLaSH.Core.Type

intPrimTyConKey, addrPrimTyConKey, eqTyConKey, listTyConKey :: Integer
tySuperKindTyConKey, unliftedTypeKindTyConKey, liftedTypeKindTyConKey, constraintKindTyConKey, typeNatKindConNameKey, typeSymbolKindConNameKey :: Integer

intPrimTyConKey          = toInteger . getKey $ PrelNames.intPrimTyConKey
addrPrimTyConKey         = toInteger . getKey $ PrelNames.addrPrimTyConKey
eqTyConKey               = toInteger . getKey $ PrelNames.eqTyConKey
listTyConKey             = toInteger . getKey $ PrelNames.listTyConKey

tySuperKindTyConKey      = toInteger . getKey $ CPrelNames.tySuperKindTyConKey
unliftedTypeKindTyConKey = toInteger . getKey $ PrelNames.unliftedTypeKindTyConKey
liftedTypeKindTyConKey   = toInteger . getKey $ PrelNames.liftedTypeKindTyConKey
constraintKindTyConKey   = toInteger . getKey $ PrelNames.constraintKindTyConKey
typeNatKindConNameKey    = toInteger . getKey $ PrelNames.typeNatKindConNameKey
typeSymbolKindConNameKey = toInteger . getKey $ PrelNames.typeSymbolKindConNameKey


intPrimTyConName, addrPrimTyConName :: TyConName
intPrimTyConName  = makeName "GHC.Prim.Int#"  intPrimTyConKey
addrPrimTyConName = makeName "GHC.Prim.Addr#" addrPrimTyConKey

tySuperKindTyConName, liftedTypeKindTyConName, unliftedTypeKindTyConName, constraintKindTyConName, typeNatKindTyCon, typeSymbolKindTyCon :: TyConName
tySuperKindTyConName      = makeName "BOX"        tySuperKindTyConKey
liftedTypeKindTyConName   = makeName "*"          liftedTypeKindTyConKey
unliftedTypeKindTyConName = makeName "#"          unliftedTypeKindTyConKey
constraintKindTyConName   = makeName "Constraint" constraintKindTyConKey
typeNatKindTyCon          = makeName "Nat"        typeNatKindConNameKey
typeSymbolKindTyCon       = makeName "Symbol"     typeSymbolKindConNameKey

intPrimTyCon :: TyCon
intPrimTyCon  = pcPrimTyCon0 intPrimTyConName IntRep

addrPrimTyCon :: TyCon
addrPrimTyCon = pcPrimTyCon0 addrPrimTyConName AddrRep

liftedTypeKind, unliftedTypeKind :: Kind
unliftedTypeKind = kindTyConType unliftedTypeKindTyCon
liftedTypeKind   = kindTyConType liftedTypeKindTyCon

tySuperKind :: Kind
tySuperKind = kindTyConType tySuperKindTyCon

tySuperKindTyCon, constraintKindTyCon, liftedTypeKindTyCon, unliftedTypeKindTyCon  :: TyCon
tySuperKindTyCon      = mkSuperKindTyCon tySuperKindTyConName
unliftedTypeKindTyCon = mkKindTyCon unliftedTypeKindTyConName tySuperKind
liftedTypeKindTyCon   = mkKindTyCon liftedTypeKindTyConName tySuperKind
constraintKindTyCon   = mkKindTyCon constraintKindTyConName tySuperKind

typeNatKind, typeSymbolKind :: Kind
typeNatKind    = kindTyConType (mkKindTyCon typeNatKindTyCon tySuperKind)
typeSymbolKind = kindTyConType (mkKindTyCon typeSymbolKindTyCon tySuperKind)

pcPrimTyCon0 ::
  TyConName
  -> PrimRep
  -> TyCon
pcPrimTyCon0 name rep
  = mkPrimTyCon name result_kind 0 rep
  where
    result_kind = unliftedTypeKind

intPrimTy :: Type
intPrimTy = mkTyConTy intPrimTyCon

addrPrimTy :: Type
addrPrimTy = mkTyConTy addrPrimTyCon

kindTyConType :: TyCon -> Type
kindTyConType = mkTyConTy
