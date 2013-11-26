-- | Builtin Type and Kind definitions
module CLaSH.Core.TysPrim
  ( liftedTypeKind
  , typeNatKind
  , typeSymbolKind
  , intPrimTy
  , voidPrimTy
  )
where

import                Unbound.LocallyNameless (string2Name)

import                CLaSH.Core.TyCon
import {-# SOURCE #-} CLaSH.Core.Type

-- | Builtin Name
tySuperKindTyConName, liftedTypeKindTyConName, typeNatKindTyConName, typeSymbolKindTyConName :: TyConName
tySuperKindTyConName      = string2Name "BOX"
liftedTypeKindTyConName   = string2Name "*"
typeNatKindTyConName      = string2Name "Nat"
typeSymbolKindTyConName   = string2Name "Symbol"

-- | Builtin Kind
liftedTypeKind, tySuperKind, typeNatKind, typeSymbolKind :: Kind
tySuperKind    = mkTyConTy (SuperKindTyCon tySuperKindTyConName)
liftedTypeKind = mkTyConTy (mkKindTyCon liftedTypeKindTyConName tySuperKind)
typeNatKind    = mkTyConTy (mkKindTyCon typeNatKindTyConName tySuperKind)
typeSymbolKind = mkTyConTy (mkKindTyCon typeSymbolKindTyConName tySuperKind)

intPrimTyConName, voidPrimTyConName :: TyConName
intPrimTyConName  = string2Name "Int"
voidPrimTyConName = string2Name "VOID"

liftedPrimTC ::
  TyConName
  -> PrimRep
  -> TyCon
liftedPrimTC name = PrimTyCon name liftedTypeKind 0

-- | Builtin Type
intPrimTy, voidPrimTy :: Type
intPrimTy  = mkTyConTy (liftedPrimTC intPrimTyConName  IntRep )
voidPrimTy = mkTyConTy (liftedPrimTC voidPrimTyConName VoidRep)
