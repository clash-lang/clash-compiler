module CLaSH.Core.TysPrim
  ( liftedTypeKind
  , typeNatKind
  , typeSymbolKind
  , intPrimTy
  , voidPrimTy
  , kindTyConType
  )
where

import Unbound.LocallyNameless (string2Name)

import CLaSH.Core.TyCon
import {-# SOURCE #-} CLaSH.Core.Type

tySuperKindTyConName, liftedTypeKindTyConName, typeNatKindTyConName, typeSymbolKindTyConName :: TyConName
tySuperKindTyConName      = string2Name "__BOX__"
liftedTypeKindTyConName   = string2Name "__*__"
typeNatKindTyConName      = string2Name "__Nat__"
typeSymbolKindTyConName   = string2Name "__Symbol__"

liftedTypeKind, tySuperKind, typeNatKind, typeSymbolKind :: Kind
liftedTypeKind = kindTyConType (mkKindTyCon liftedTypeKindTyConName tySuperKind)
tySuperKind    = kindTyConType (mkSuperKindTyCon tySuperKindTyConName)
typeNatKind    = kindTyConType (mkKindTyCon typeNatKindTyConName tySuperKind)
typeSymbolKind = kindTyConType (mkKindTyCon typeSymbolKindTyConName tySuperKind)

intPrimTyConName, voidPrimTyConName :: TyConName
intPrimTyConName  = string2Name "__Int#__"
voidPrimTyConName = string2Name "__VOID__"

pcPrimTyCon0 ::
  TyConName
  -> PrimRep
  -> TyCon
pcPrimTyCon0 name rep = mkPrimTyCon name liftedTypeKind 0 rep

intPrimTy, voidPrimTy :: Type
intPrimTy  = mkTyConTy (pcPrimTyCon0 intPrimTyConName  IntRep )
voidPrimTy = mkTyConTy (pcPrimTyCon0 voidPrimTyConName VoidRep)

kindTyConType :: TyCon -> Type
kindTyConType = mkTyConTy
