{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016     , Myrtle Software Ltd
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Builtin Type and Kind definitions
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Core.TysPrim
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
  , byteArrayPrimTy
  , typeNatSubTyFamName
  , tysPrimMap
  )
where

import qualified Data.List            as List

import           PrelNames
import           Unique               (getKey)

import           Clash.Core.Name
import           Clash.Core.TyCon
import {-# SOURCE #-} Clash.Core.Type
import           Clash.Unique

-- | Builtin Name
tySuperKindTyConName, liftedTypeKindTyConName, typeNatKindTyConName, typeSymbolKindTyConName :: TyConName
tySuperKindTyConName      = mkUnsafeSystemName "tYPE" (getKey tYPETyConKey)
liftedTypeKindTyConName   = mkUnsafeSystemName "*" (getKey liftedTypeKindTyConKey)
typeNatKindTyConName      = mkUnsafeSystemName "Nat" (getKey typeNatKindConNameKey)
typeSymbolKindTyConName   = mkUnsafeSystemName "Symbol" (getKey typeSymbolKindConNameKey)

typeNatSubTyFamName :: TyConName
typeNatSubTyFamName = mkUnsafeSystemName "GHC.TypeNats.-" (getKey typeNatSubTyFamNameKey)

-- | Builtin Kind
liftedTypeKindTc, tySuperKindTc, typeNatKindTc, typeSymbolKindTc :: TyCon
tySuperKindTc    = SuperKindTyCon (nameUniq tySuperKindTyConName) tySuperKindTyConName
liftedTypeKindTc = mkKindTyCon liftedTypeKindTyConName tySuperKind
typeNatKindTc    = mkKindTyCon typeNatKindTyConName tySuperKind
typeSymbolKindTc = mkKindTyCon typeSymbolKindTyConName tySuperKind

liftedTypeKind, tySuperKind, typeNatKind, typeSymbolKind :: Type
tySuperKind    = mkTyConTy tySuperKindTyConName
liftedTypeKind = mkTyConTy liftedTypeKindTyConName
typeNatKind    = mkTyConTy typeNatKindTyConName
typeSymbolKind = mkTyConTy typeSymbolKindTyConName

intPrimTyConName, integerPrimTyConName, charPrimTyConName, stringPrimTyConName,
  voidPrimTyConName, wordPrimTyConName, int64PrimTyConName,
  word64PrimTyConName, floatPrimTyConName, doublePrimTyConName,
  naturalPrimTyConName, byteArrayPrimTyConName :: TyConName
intPrimTyConName     = mkUnsafeSystemName "GHC.Prim.Int#"
                                (getKey intPrimTyConKey)
integerPrimTyConName = mkUnsafeSystemName "GHC.Integer.Type.Integer"
                                (getKey integerTyConKey)
stringPrimTyConName  = mkUnsafeSystemName "GHC.Prim.Addr#" (getKey addrPrimTyConKey)
charPrimTyConName    = mkUnsafeSystemName "GHC.Prim.Char#"
                                (getKey charPrimTyConKey)
voidPrimTyConName    = mkUnsafeSystemName "Void#" (getKey voidPrimTyConKey)
wordPrimTyConName    = mkUnsafeSystemName "GHC.Prim.Word#"
                                (getKey wordPrimTyConKey)
int64PrimTyConName   = mkUnsafeSystemName "GHC.Prim.Int64#"
                                (getKey int64PrimTyConKey)
word64PrimTyConName  = mkUnsafeSystemName "GHC.Prim.Word64#"
                                (getKey word64PrimTyConKey)
floatPrimTyConName   = mkUnsafeSystemName "GHC.Prim.Float#"
                                (getKey floatPrimTyConKey)
doublePrimTyConName  = mkUnsafeSystemName "GHC.Prim.Double#"
                                (getKey doublePrimTyConKey)
naturalPrimTyConName = mkUnsafeSystemName "GHC.Natural.Natural"
                                (getKey naturalTyConKey)
byteArrayPrimTyConName = mkUnsafeSystemName "GHC.Prim.ByteArray#"
                          (getKey byteArrayPrimTyConKey)

liftedPrimTC :: TyConName
             -> TyCon
liftedPrimTC name = PrimTyCon (nameUniq name) name liftedTypeKind 0

-- | Builtin Type
intPrimTc, integerPrimTc, charPrimTc, stringPrimTc, voidPrimTc, wordPrimTc,
  int64PrimTc, word64PrimTc, floatPrimTc, doublePrimTc, naturalPrimTc,
  byteArrayPrimTc :: TyCon
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
byteArrayPrimTc = liftedPrimTC  byteArrayPrimTyConName

intPrimTy, integerPrimTy, charPrimTy, stringPrimTy, voidPrimTy, wordPrimTy,
  int64PrimTy, word64PrimTy, floatPrimTy, doublePrimTy, naturalPrimTy,
  byteArrayPrimTy :: Type
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
byteArrayPrimTy = mkTyConTy byteArrayPrimTyConName

tysPrimMap :: TyConMap
tysPrimMap = List.foldl' (\s (k,x) -> extendUniqMap k x s) emptyUniqMap
  [  (tySuperKindTyConName , tySuperKindTc)
  ,  (liftedTypeKindTyConName , liftedTypeKindTc)
  ,  (typeNatKindTyConName , typeNatKindTc)
  ,  (typeSymbolKindTyConName , typeSymbolKindTc)
  ,  (intPrimTyConName , intPrimTc)
  ,  (integerPrimTyConName , integerPrimTc)
  ,  (charPrimTyConName , charPrimTc)
  ,  (stringPrimTyConName , stringPrimTc)
  ,  (voidPrimTyConName , voidPrimTc)
  ,  (wordPrimTyConName , wordPrimTc)
  ,  (int64PrimTyConName , int64PrimTc)
  ,  (word64PrimTyConName , word64PrimTc)
  ,  (floatPrimTyConName , floatPrimTc)
  ,  (doublePrimTyConName , doublePrimTc)
  ,  (naturalPrimTyConName , naturalPrimTc)
  ,  (byteArrayPrimTyConName , byteArrayPrimTc)
  ]
