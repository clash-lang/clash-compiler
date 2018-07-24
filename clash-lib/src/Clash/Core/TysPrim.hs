{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016     , Myrtle Software Ltd
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Builtin Type and Kind definitions
-}

{-# LANGUAGE CPP #-}

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
  , tysPrimMap
  )
where

import           Control.Arrow                    (first)
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as HashMap

import           PrelNames
import           Unique                           (Unique, getKey)

import           Clash.Core.Name
import           Clash.Core.TyCon
import {-# SOURCE #-} Clash.Core.Type

-- | Builtin Name
tySuperKindTyConName, liftedTypeKindTyConName, typeNatKindTyConName, typeSymbolKindTyConName :: TyConName
tySuperKindTyConName      = string2SystemName "BOX"
liftedTypeKindTyConName   = string2SystemName "*"
typeNatKindTyConName      = string2SystemName "Nat"
typeSymbolKindTyConName   = string2SystemName "Symbol"

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
  naturalPrimTyConName, byteArrayPrimTyConName :: TyConName
intPrimTyConName     = makeSystemName "GHC.Prim.Int#"
                                (uniqueToInteger intPrimTyConKey)
integerPrimTyConName = makeSystemName "GHC.Integer.Type.Integer"
                                (uniqueToInteger integerTyConKey)
stringPrimTyConName  = string2SystemName "String"
charPrimTyConName    = makeSystemName "GHC.Prim.Char#"
                                (uniqueToInteger charPrimTyConKey)
voidPrimTyConName    = string2SystemName "VOID"
wordPrimTyConName    = makeSystemName "GHC.Prim.Word#"
                                (uniqueToInteger wordPrimTyConKey)
int64PrimTyConName   = makeSystemName "GHC.Prim.Int64#"
                                (uniqueToInteger int64PrimTyConKey)
word64PrimTyConName  = makeSystemName "GHC.Prim.Word64#"
                                (uniqueToInteger word64PrimTyConKey)
floatPrimTyConName   = makeSystemName "GHC.Prim.Float#"
                                (uniqueToInteger floatPrimTyConKey)
doublePrimTyConName  = makeSystemName "GHC.Prim.Double#"
                                (uniqueToInteger doublePrimTyConKey)
#if MIN_VERSION_ghc(8,2,0)
naturalPrimTyConName = makeSystemName "GHC.Natural.Natural"
                                (uniqueToInteger naturalTyConKey)
#else
naturalPrimTyConName = string2SystemName "GHC.Natural.Natural"
#endif
byteArrayPrimTyConName = makeSystemName "GHC.Prim.ByteArray#"
                          (uniqueToInteger byteArrayPrimTyConKey)

liftedPrimTC :: TyConName
             -> TyCon
liftedPrimTC name = PrimTyCon name liftedTypeKind 0

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

tysPrimMap :: HashMap TyConOccName TyCon
tysPrimMap = HashMap.fromList $ map (first nameOcc)
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
  , (byteArrayPrimTyConName,byteArrayPrimTc)
  ]
