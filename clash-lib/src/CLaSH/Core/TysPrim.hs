-- | Builtin Type and Kind definitions
module CLaSH.Core.TysPrim
  ( liftedTypeKind
  , typeNatKind
  , typeSymbolKind
  , intPrimTy
  , integerPrimTy
  , charPrimTy
  , stringPrimTy
  , voidPrimTy
  , wordPrimTy
  , int64PrimTy
  , word64PrimTy
  , tysPrimMap
  )
where

import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as HashMap
import           Unbound.Generics.LocallyNameless (string2Name)

import           CLaSH.Core.TyCon
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


intPrimTyConName, integerPrimTyConName, charPrimTyConName, stringPrimTyConName,
  voidPrimTyConName, wordPrimTyConName, int64PrimTyConName,
  word64PrimTyConName :: TyConName
intPrimTyConName     = string2Name "GHC.Prim.Int#"
integerPrimTyConName = string2Name "GHC.Integer.Type.Integer"
stringPrimTyConName  = string2Name "String"
charPrimTyConName    = string2Name "GHC.Prim.Char#"
voidPrimTyConName    = string2Name "VOID"
wordPrimTyConName    = string2Name "GHC.Prim.Word#"
int64PrimTyConName   = string2Name "GHC.Prim.Int64#"
word64PrimTyConName  = string2Name "GHC.Prim.Word64#"

liftedPrimTC :: TyConName
             -> TyCon
liftedPrimTC name = PrimTyCon name liftedTypeKind 0

-- | Builtin Type
intPrimTc, integerPrimTc, charPrimTc, stringPrimTc, voidPrimTc, wordPrimTc,
  int64PrimTc, word64PrimTc :: TyCon
intPrimTc     = liftedPrimTC intPrimTyConName
integerPrimTc = liftedPrimTC integerPrimTyConName
charPrimTc    = liftedPrimTC charPrimTyConName
stringPrimTc  = liftedPrimTC stringPrimTyConName
voidPrimTc    = liftedPrimTC voidPrimTyConName
wordPrimTc    = liftedPrimTC wordPrimTyConName
int64PrimTc   = liftedPrimTC int64PrimTyConName
word64PrimTc  = liftedPrimTC word64PrimTyConName

intPrimTy, integerPrimTy, charPrimTy, stringPrimTy, voidPrimTy, wordPrimTy,
  int64PrimTy, word64PrimTy :: Type
intPrimTy     = mkTyConTy intPrimTyConName
integerPrimTy = mkTyConTy integerPrimTyConName
charPrimTy    = mkTyConTy charPrimTyConName
stringPrimTy  = mkTyConTy stringPrimTyConName
voidPrimTy    = mkTyConTy voidPrimTyConName
wordPrimTy    = mkTyConTy wordPrimTyConName
int64PrimTy   = mkTyConTy int64PrimTyConName
word64PrimTy  = mkTyConTy word64PrimTyConName

tysPrimMap :: HashMap TyConName TyCon
tysPrimMap = HashMap.fromList
  [ (tySuperKindTyConName,tySuperKindtc)
  , (liftedTypeKindTyConName,liftedTypeKindtc)
  , (typeNatKindTyConName,typeNatKindtc)
  , (typeSymbolKindTyConName,typeSymbolKindtc)
  , (intPrimTyConName,intPrimTc)
  , (integerPrimTyConName,integerPrimTc)
  , (charPrimTyConName,charPrimTc)
  , (stringPrimTyConName,stringPrimTc)
  , (voidPrimTyConName,voidPrimTc)
  , (wordPrimTyConName,wordPrimTc)
  , (int64PrimTyConName,int64PrimTc)
  , (word64PrimTyConName,word64PrimTc)
  ]
