{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016     , Myrtle Software Ltd,
                     2021     , QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

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
  , eqPrimTy
  , tysPrimMap
  )
where


#if MIN_VERSION_ghc(9,0,0)
import           GHC.Builtin.Names
import           GHC.Types.Unique     (getKey)
#else
import           PrelNames
import           Unique               (getKey)
#endif

import           Clash.Core.Name
import           Clash.Core.TyCon
import           Clash.Core.Type
import           Clash.Core.Var (mkTyVar)
import qualified Clash.Data.UniqMap as UniqMap

-- | Builtin Name
liftedTypeKindTyConName, typeNatKindTyConName, typeSymbolKindTyConName :: TyConName
liftedTypeKindTyConName   = mkUnsafeSystemName "Type" (getKey liftedTypeKindTyConKey)
typeNatKindTyConName      = mkUnsafeSystemName "Nat" (getKey typeNatKindConNameKey)
typeSymbolKindTyConName   = mkUnsafeSystemName "Symbol" (getKey typeSymbolKindConNameKey)

-- | Builtin Kind
liftedTypeKindTc, typeNatKindTc, typeSymbolKindTc :: TyCon
liftedTypeKindTc = mkKindTyCon liftedTypeKindTyConName liftedTypeKind
typeNatKindTc    = mkKindTyCon typeNatKindTyConName liftedTypeKind
typeSymbolKindTc = mkKindTyCon typeSymbolKindTyConName liftedTypeKind

liftedTypeKind, typeNatKind, typeSymbolKind :: Type
liftedTypeKind = mkTyConTy liftedTypeKindTyConName
typeNatKind    = mkTyConTy typeNatKindTyConName
typeSymbolKind = mkTyConTy typeSymbolKindTyConName

intPrimTyConName, integerPrimTyConName, charPrimTyConName, stringPrimTyConName,
  voidPrimTyConName, wordPrimTyConName, int64PrimTyConName,
  word64PrimTyConName, floatPrimTyConName, doublePrimTyConName,
  naturalPrimTyConName, byteArrayPrimTyConName, eqPrimTyConName :: TyConName
intPrimTyConName     = mkUnsafeSystemName "GHC.Prim.Int#"
                                (getKey intPrimTyConKey)
#if MIN_VERSION_base(4,15,0)
integerPrimTyConName = mkUnsafeSystemName "GHC.Num.Integer.Integer"
                                (getKey integerTyConKey)
#else
integerPrimTyConName = mkUnsafeSystemName "GHC.Integer.Type.Integer"
                                (getKey integerTyConKey)
#endif
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
#if MIN_VERSION_base(4,15,0)
naturalPrimTyConName = mkUnsafeSystemName "GHC.Num.Natural.Natural"
                                (getKey naturalTyConKey)
#else
naturalPrimTyConName = mkUnsafeSystemName "GHC.Natural.Natural"
                                (getKey naturalTyConKey)
#endif
byteArrayPrimTyConName = mkUnsafeSystemName "GHC.Prim.ByteArray#"
                          (getKey byteArrayPrimTyConKey)

eqPrimTyConName = mkUnsafeSystemName "GHC.Prim.~#" (getKey eqPrimTyConKey)

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

eqPrimTc :: TyCon
eqPrimTc = PrimTyCon (nameUniq eqPrimTyConName) eqPrimTyConName ty 4
 where
  -- forall (a :: Type). forall (b :: Type). a -> b -> Type
  --
  -- The "real" type for this in GHC has a codomain of `TYPE ('TupleRep '[])`
  -- instead of the `TYPE 'LiftedRep` used here.
  ty  = mkPolyFunTy liftedTypeKind
    [Left aTv, Left bTv, Right (VarTy aTv), Right (VarTy bTv)]

  aTv = mkTyVar liftedTypeKind (mkUnsafeSystemName "a" 0)
  bTv = mkTyVar liftedTypeKind (mkUnsafeSystemName "b" 1)

intPrimTy, integerPrimTy, charPrimTy, stringPrimTy, voidPrimTy, wordPrimTy,
  int64PrimTy, word64PrimTy, floatPrimTy, doublePrimTy, naturalPrimTy,
  byteArrayPrimTy, eqPrimTy :: Type
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
eqPrimTy = mkTyConTy eqPrimTyConName

tysPrimMap :: TyConMap
tysPrimMap = UniqMap.fromList
  [  (liftedTypeKindTyConName , liftedTypeKindTc)
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
  ,  (eqPrimTyConName , eqPrimTc)
  ]
