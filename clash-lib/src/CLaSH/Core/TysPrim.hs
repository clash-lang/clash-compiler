{-|
  Copyright   :  (C) 2012-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Builtin Type and Kind definitions
-}

{-# LANGUAGE CPP #-}

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
  , floatPrimTy
  , doublePrimTy
  , naturalPrimTy
  , tysPrimMap
  )
where

import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as HashMap
import           Unbound.Generics.LocallyNameless (makeName, string2Name)

import           PrelNames
import           Unique                           (Unique, getKey)

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

uniqueToInteger :: Unique -> Integer
uniqueToInteger = toInteger . getKey

intPrimTyConName, integerPrimTyConName, charPrimTyConName, stringPrimTyConName,
  voidPrimTyConName, wordPrimTyConName, int64PrimTyConName,
  word64PrimTyConName, floatPrimTyConName, doublePrimTyConName,
  naturalPrimTyConName :: TyConName
intPrimTyConName     = makeName "GHC.Prim.Int#"
                                (uniqueToInteger intPrimTyConKey)
integerPrimTyConName = makeName "GHC.Integer.Type.Integer"
                                (uniqueToInteger integerTyConKey)
stringPrimTyConName  = string2Name "String"
charPrimTyConName    = makeName "GHC.Prim.Char#"
                                (uniqueToInteger charPrimTyConKey)
voidPrimTyConName    = string2Name "VOID"
wordPrimTyConName    = makeName "GHC.Prim.Word#"
                                (uniqueToInteger wordPrimTyConKey)
int64PrimTyConName   = makeName "GHC.Prim.Int64#"
                                (uniqueToInteger int64PrimTyConKey)
word64PrimTyConName  = makeName "GHC.Prim.Word64#"
                                (uniqueToInteger word64PrimTyConKey)
floatPrimTyConName   = makeName "GHC.Prim.Float#"
                                (uniqueToInteger floatPrimTyConKey)
doublePrimTyConName  = makeName "GHC.Prim.Double#"
                                (uniqueToInteger doublePrimTyConKey)
#if MIN_VERSION_ghc(8,2,0)
naturalPrimTyConName = makeName "GHC.Natural.Natural"
                                (uniqueToInteger naturalTyConKey)
#else
naturalPrimTyConName = string2Name "GHC.Natural.Natural"
#endif

liftedPrimTC :: TyConName
             -> TyCon
liftedPrimTC name = PrimTyCon name liftedTypeKind 0

-- | Builtin Type
intPrimTc, integerPrimTc, charPrimTc, stringPrimTc, voidPrimTc, wordPrimTc,
  int64PrimTc, word64PrimTc, floatPrimTc, doublePrimTc, naturalPrimTc :: TyCon
intPrimTc     = liftedPrimTC intPrimTyConName
integerPrimTc = liftedPrimTC integerPrimTyConName
charPrimTc    = liftedPrimTC charPrimTyConName
stringPrimTc  = liftedPrimTC stringPrimTyConName
voidPrimTc    = liftedPrimTC voidPrimTyConName
wordPrimTc    = liftedPrimTC wordPrimTyConName
int64PrimTc   = liftedPrimTC int64PrimTyConName
word64PrimTc  = liftedPrimTC word64PrimTyConName
floatPrimTc   = liftedPrimTC floatPrimTyConName
doublePrimTc  = liftedPrimTC doublePrimTyConName
naturalPrimTc = liftedPrimTC naturalPrimTyConName

intPrimTy, integerPrimTy, charPrimTy, stringPrimTy, voidPrimTy, wordPrimTy,
  int64PrimTy, word64PrimTy, floatPrimTy, doublePrimTy, naturalPrimTy :: Type
intPrimTy     = mkTyConTy intPrimTyConName
integerPrimTy = mkTyConTy integerPrimTyConName
charPrimTy    = mkTyConTy charPrimTyConName
stringPrimTy  = mkTyConTy stringPrimTyConName
voidPrimTy    = mkTyConTy voidPrimTyConName
wordPrimTy    = mkTyConTy wordPrimTyConName
int64PrimTy   = mkTyConTy int64PrimTyConName
word64PrimTy  = mkTyConTy word64PrimTyConName
floatPrimTy   = mkTyConTy floatPrimTyConName
doublePrimTy  = mkTyConTy doublePrimTyConName
naturalPrimTy = mkTyConTy naturalPrimTyConName

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
  , (floatPrimTyConName,floatPrimTc)
  , (doublePrimTyConName,doublePrimTc)
  , (naturalPrimTyConName,naturalPrimTc)
  ]
