-- | Builtin Type and Kind definitions
module CLaSH.Core.TysPrim
  ( liftedTypeKind
  , typeNatKind
  , typeSymbolKind
  , intPrimTy
  , voidPrimTy
  , tysPrimMap
  )
where

import                Data.HashMap.Strict     (HashMap)
import qualified      Data.HashMap.Strict     as HashMap
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
liftedTypeKindtc, tySuperKindtc, typeNatKindtc, typeSymbolKindtc :: TyCon
tySuperKindtc    = SuperKindTyCon tySuperKindTyConName
liftedTypeKindtc = mkKindTyCon liftedTypeKindTyConName tySuperKind
typeNatKindtc    = mkKindTyCon typeNatKindTyConName tySuperKind
typeSymbolKindtc = mkKindTyCon typeSymbolKindTyConName tySuperKind

liftedTypeKind, tySuperKind, typeNatKind, typeSymbolKind :: Type
tySuperKind    = mkTyConTy tySuperKindTyConName
liftedTypeKind = mkTyConTy liftedTypeKindTyConName
typeNatKind    = mkTyConTy typeNatKindTyConName
typeSymbolKind = mkTyConTy typeSymbolKindTyConName


intPrimTyConName, voidPrimTyConName :: TyConName
intPrimTyConName  = string2Name "Int"
voidPrimTyConName = string2Name "VOID"

liftedPrimTC :: TyConName
             -> TyCon
liftedPrimTC name = PrimTyCon name liftedTypeKind 0

-- | Builtin Type
intPrimTc, voidPrimTc :: TyCon
intPrimTc  = (liftedPrimTC intPrimTyConName )
voidPrimTc = (liftedPrimTC voidPrimTyConName)

intPrimTy, voidPrimTy :: Type
intPrimTy  = mkTyConTy intPrimTyConName
voidPrimTy = mkTyConTy voidPrimTyConName

tysPrimMap :: HashMap TyConName TyCon
tysPrimMap = HashMap.fromList
  [ (tySuperKindTyConName,tySuperKindtc)
  , (liftedTypeKindTyConName,liftedTypeKindtc)
  , (typeNatKindTyConName,typeNatKindtc)
  , (typeSymbolKindTyConName,typeSymbolKindtc)
  , (intPrimTyConName,intPrimTc)
  , (voidPrimTyConName,voidPrimTc)
  ]
