module CLaSH.Core.TysPrim where

-- External Modules
import Unbound.LocallyNameless         (makeName)

-- GHC API
import PrelNames (intPrimTyConKey,addrPrimTyConKey,unliftedTypeKindTyConKey
                 ,tySuperKindTyConKey)
import Unique    (getKey)

import CLaSH.Core.TyCon
import CLaSH.Core.TypeRep

intPrimTyConName, addrPrimTyConName :: TyConName
intPrimTyConName  = makeName "#Int"  (toInteger . getKey $ intPrimTyConKey)
addrPrimTyConName = makeName "#Addr" (toInteger . getKey $ addrPrimTyConKey)

tySuperKindTyConName, unliftedTypeKindTyConName :: TyConName
tySuperKindTyConName      = makeName "BOX" (toInteger . getKey $ tySuperKindTyConKey)
unliftedTypeKindTyConName = makeName "#" (toInteger . getKey $ unliftedTypeKindTyConKey)

intPrimTyCon :: TyCon
intPrimTyCon  = pcPrimTyCon0 intPrimTyConName IntRep

addrPrimTyCon :: TyCon
addrPrimTyCon = pcPrimTyCon0 addrPrimTyConName AddrRep

unliftedTypeKindTyCon :: TyCon
unliftedTypeKindTyCon = mkKindTyCon unliftedTypeKindTyConName tySuperKind

unliftedTypeKind :: Kind
unliftedTypeKind = kindTyConType unliftedTypeKindTyCon

tySuperKind :: SuperKind
tySuperKind = kindTyConType tySuperKindTyCon

tySuperKindTyCon :: TyCon
tySuperKindTyCon = mkSuperKindTyCon tySuperKindTyConName

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
kindTyConType kind = TyConApp kind []
