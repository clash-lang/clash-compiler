{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016     , Myrtle Software Ltd,
                     2021-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Builtin Type and Kind definitions. Their type constructors, and their names,
  are those of "Clash.Core.Scoped.TysPrim".
-}

module Clash.Core.TysPrim
  ( liftedTypeKind
  , typeNatKind
  , typeSymbolKind
  , intPrimTy
  , integerPrimTy
  , charPrimTy
  , stringPrimTy
  , wordPrimTy
  , int64PrimTy
  , word64PrimTy
  , int8PrimTy
  , int16PrimTy
  , int32PrimTy
  , word8PrimTy
  , word16PrimTy
  , word32PrimTy
  , floatPrimTy
  , doublePrimTy
  , naturalPrimTy
  , byteArrayPrimTy
  , eqPrimTy
  , tysPrimMap
  , integerIsDc
  , naturalNsDc
  )
where

import           Clash.Core.Type (Type, mkTyConTy)

import           Clash.Core.Scoped.TysPrim
  (tysPrimMap, integerIsDc, naturalNsDc, liftedTypeKindTyConName,
   typeNatKindTyConName, typeSymbolKindTyConName, intPrimTyConName,
   integerPrimTyConName, charPrimTyConName, stringPrimTyConName,
   wordPrimTyConName, int64PrimTyConName, word64PrimTyConName,
   int8PrimTyConName, int16PrimTyConName, int32PrimTyConName,
   word8PrimTyConName, word16PrimTyConName, word32PrimTyConName,
   floatPrimTyConName, doublePrimTyConName, naturalPrimTyConName,
   byteArrayPrimTyConName, eqPrimTyConName)

-- | Builtin Kind
liftedTypeKind, typeNatKind, typeSymbolKind :: Type
liftedTypeKind = mkTyConTy liftedTypeKindTyConName
typeNatKind    = mkTyConTy typeNatKindTyConName
typeSymbolKind = mkTyConTy typeSymbolKindTyConName

-- | Builtin Type
intPrimTy, integerPrimTy, charPrimTy, stringPrimTy, wordPrimTy,
  int64PrimTy, word64PrimTy, floatPrimTy, doublePrimTy, naturalPrimTy,
  byteArrayPrimTy, eqPrimTy :: Type
intPrimTy     = mkTyConTy intPrimTyConName
integerPrimTy = mkTyConTy integerPrimTyConName
charPrimTy    = mkTyConTy charPrimTyConName
stringPrimTy  = mkTyConTy stringPrimTyConName
wordPrimTy    = mkTyConTy wordPrimTyConName
int64PrimTy   = mkTyConTy int64PrimTyConName
word64PrimTy  = mkTyConTy word64PrimTyConName
floatPrimTy   = mkTyConTy floatPrimTyConName
doublePrimTy  = mkTyConTy doublePrimTyConName
naturalPrimTy = mkTyConTy naturalPrimTyConName
byteArrayPrimTy = mkTyConTy byteArrayPrimTyConName
eqPrimTy = mkTyConTy eqPrimTyConName

int8PrimTy, int16PrimTy, int32PrimTy, word8PrimTy, word16PrimTy,
  word32PrimTy :: Type
int8PrimTy    = mkTyConTy int8PrimTyConName
int16PrimTy   = mkTyConTy int16PrimTyConName
int32PrimTy   = mkTyConTy int32PrimTyConName
word8PrimTy   = mkTyConTy word8PrimTyConName
word16PrimTy  = mkTyConTy word16PrimTyConName
word32PrimTy  = mkTyConTy word32PrimTyConName
