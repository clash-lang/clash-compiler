{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016     , Myrtle Software Ltd,
                     2021     , QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Builtin Type and Kind definitions
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Core.TysPrim
  ( liftedTypeKind
  , typeNatKind
  , typeSymbolKind
  , intPrimTy
  , integerPrimTy
  , charPrimTy
  , stringPrimTy
#if !MIN_VERSION_ghc(9,2,0)
  , voidPrimTy
#endif
  , wordPrimTy
  , int64PrimTy
  , word64PrimTy
#if MIN_VERSION_ghc(8,8,0)
  , int8PrimTy
  , int16PrimTy
  , int32PrimTy
  , word8PrimTy
  , word16PrimTy
  , word32PrimTy
#endif
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

#if MIN_VERSION_ghc(8,8,0)
import           GHC.Base hiding (Type, TyCon)
import           Data.Text.Extra (showt)
#endif

#if MIN_VERSION_base(4,17,0)
import           Clash.Core.DataCon (DataCon(..), DcStrictness(..))
import           GHC.Num.Integer (Integer(..))
import           GHC.Num.Natural (Natural(..))
#endif

import           Clash.Core.Name
import           Clash.Core.TyCon
import           Clash.Core.Type
import           Clash.Core.Var (mkTyVar)
import qualified Clash.Data.UniqMap as UniqMap

-- | Builtin Name
liftedTypeKindTyConName, typeNatKindTyConName, typeSymbolKindTyConName :: TyConName
liftedTypeKindTyConName   = mkUnsafeSystemName "Type" (getKey liftedTypeKindTyConKey)
#if MIN_VERSION_ghc(9,2,0)
typeNatKindTyConName      = naturalPrimTyConName
#else
typeNatKindTyConName      = mkUnsafeSystemName "Nat" (getKey typeNatKindConNameKey)
#endif
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
  wordPrimTyConName,  int64PrimTyConName, word64PrimTyConName,
  floatPrimTyConName, doublePrimTyConName,
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

#if !MIN_VERSION_ghc(9,2,0)
voidPrimTyConName :: TyConName
voidPrimTyConName    = mkUnsafeSystemName "Void#" (getKey voidPrimTyConKey)
#endif

#if MIN_VERSION_ghc(8,8,0)
int8PrimTyConName, int16PrimTyConName, int32PrimTyConName, word8PrimTyConName,
  word16PrimTyConName, word32PrimTyConName :: TyConName
int8PrimTyConName   = mkUnsafeSystemName (showt ''Int8#) (getKey int8PrimTyConKey)
int16PrimTyConName  = mkUnsafeSystemName (showt ''Int16#) (getKey int16PrimTyConKey)
int32PrimTyConName  = mkUnsafeSystemName (showt ''Int32#) (getKey int32PrimTyConKey)
word8PrimTyConName  = mkUnsafeSystemName (showt ''Word8#) (getKey word8PrimTyConKey)
word16PrimTyConName = mkUnsafeSystemName (showt ''Word16#) (getKey word16PrimTyConKey)
word32PrimTyConName = mkUnsafeSystemName (showt ''Word32#) (getKey word32PrimTyConKey)
#endif

liftedPrimTC :: TyConName
             -> TyCon
liftedPrimTC name = PrimTyCon (nameUniq name) name liftedTypeKind 0

-- | Builtin Type
intPrimTc, integerPrimTc, charPrimTc, stringPrimTc, wordPrimTc,
  int64PrimTc, word64PrimTc, floatPrimTc, doublePrimTc, naturalPrimTc,
  byteArrayPrimTc :: TyCon
intPrimTc     = liftedPrimTC intPrimTyConName
#if MIN_VERSION_base(4,17,0)
-- While GHC might have dropped Integer and Natural literals, in Clash it is
-- still nice to have them around. However, Integer and Natural are also no
-- longer primitive types in GHC, but we still want to give the Integer and
-- Natural type to our Integer and Natural literals.
--
-- So instead of recording the primitive types, we record the algebraic types,
-- i.e. the complete data type for Integer and Natural, data constructors and all.

integerPrimTc =
  let
    name = integerPrimTyConName
    uniq = nameUniq name
    isDcNm = mkUnsafeSystemName (showt 'IS) (getKey integerISDataConKey)
    isDc = MkData
      { dcName = isDcNm
      , dcUniq = nameUniq isDcNm
      , dcTag  = 1
      , dcType = mkPolyFunTy integerPrimTy [Right intPrimTy]
      , dcUnivTyVars = []
      , dcExtTyVars = []
      , dcArgTys = [intPrimTy]
      , dcArgStrict = [Strict]
      , dcFieldLabels = []
      }
    ipDcNm = mkUnsafeSystemName (showt 'IP) (getKey integerIPDataConKey)
    ipDc = MkData
      { dcName = ipDcNm
      , dcUniq = nameUniq ipDcNm
      , dcTag  = 2
      , dcType = mkPolyFunTy integerPrimTy [Right byteArrayPrimTy]
      , dcUnivTyVars = []
      , dcExtTyVars = []
      , dcArgTys = [byteArrayPrimTy]
      , dcArgStrict = [Strict]
      , dcFieldLabels = []
      }
    inDcNm = mkUnsafeSystemName (showt 'IN) (getKey integerINDataConKey)
    inDc = MkData
      { dcName = inDcNm
      , dcUniq = nameUniq inDcNm
      , dcTag  = 3
      , dcType = mkPolyFunTy integerPrimTy [Right byteArrayPrimTy]
      , dcUnivTyVars = []
      , dcExtTyVars = []
      , dcArgTys = [byteArrayPrimTy]
      , dcArgStrict = [Strict]
      , dcFieldLabels = []
      }
    rhs = DataTyCon [isDc,ipDc,inDc]
  in
    AlgTyCon uniq name liftedTypeKind 0 rhs False

naturalPrimTc =
  let
    name = naturalPrimTyConName
    uniq = nameUniq name
    nsDcNm = mkUnsafeSystemName (showt 'NS) (getKey naturalNSDataConKey)
    nsDc = MkData
      { dcName = nsDcNm
      , dcUniq = nameUniq nsDcNm
      , dcTag  = 1
      , dcType = mkPolyFunTy naturalPrimTy [Right wordPrimTy]
      , dcUnivTyVars = []
      , dcExtTyVars = []
      , dcArgTys = [wordPrimTy]
      , dcArgStrict = [Strict]
      , dcFieldLabels = []
      }
    nbDcNm = mkUnsafeSystemName (showt 'NB) (getKey naturalNBDataConKey)
    nbDc = MkData
      { dcName = nbDcNm
      , dcUniq = nameUniq nbDcNm
      , dcTag  = 2
      , dcType = mkPolyFunTy naturalPrimTy [Right byteArrayPrimTy]
      , dcUnivTyVars = []
      , dcExtTyVars = []
      , dcArgTys = [byteArrayPrimTy]
      , dcArgStrict = [Strict]
      , dcFieldLabels = []
      }
    rhs = DataTyCon [nsDc,nbDc]
   in
    AlgTyCon uniq name liftedTypeKind 0 rhs False
#else
integerPrimTc = liftedPrimTC integerPrimTyConName
naturalPrimTc = liftedPrimTC naturalPrimTyConName
#endif
charPrimTc    = liftedPrimTC charPrimTyConName
stringPrimTc  = liftedPrimTC stringPrimTyConName
wordPrimTc    = liftedPrimTC wordPrimTyConName
int64PrimTc   = liftedPrimTC int64PrimTyConName
word64PrimTc  = liftedPrimTC word64PrimTyConName
floatPrimTc   = liftedPrimTC floatPrimTyConName
doublePrimTc  = liftedPrimTC doublePrimTyConName
byteArrayPrimTc = liftedPrimTC  byteArrayPrimTyConName

#if !MIN_VERSION_ghc(9,2,0)
voidPrimTc :: TyCon
voidPrimTc    = liftedPrimTC voidPrimTyConName
#endif

#if MIN_VERSION_ghc(8,8,0)
int8PrimTc, int16PrimTc, int32PrimTc, word8PrimTc, word16PrimTc,
  word32PrimTc :: TyCon
int8PrimTc    = liftedPrimTC int8PrimTyConName
int16PrimTc   = liftedPrimTC int16PrimTyConName
int32PrimTc   = liftedPrimTC int32PrimTyConName
word8PrimTc   = liftedPrimTC word8PrimTyConName
word16PrimTc  = liftedPrimTC word16PrimTyConName
word32PrimTc  = liftedPrimTC word32PrimTyConName
#endif

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

#if !MIN_VERSION_ghc(9,2,0)
voidPrimTy :: Type
voidPrimTy    = mkTyConTy voidPrimTyConName
#endif

#if MIN_VERSION_ghc(8,8,0)
int8PrimTy, int16PrimTy, int32PrimTy, word8PrimTy, word16PrimTy,
  word32PrimTy :: Type
int8PrimTy    = mkTyConTy int8PrimTyConName
int16PrimTy   = mkTyConTy int16PrimTyConName
int32PrimTy   = mkTyConTy int32PrimTyConName
word8PrimTy   = mkTyConTy word8PrimTyConName
word16PrimTy  = mkTyConTy word16PrimTyConName
word32PrimTy  = mkTyConTy word32PrimTyConName
#endif

tysPrimMap :: TyConMap
tysPrimMap = UniqMap.fromList
  [  (liftedTypeKindTyConName , liftedTypeKindTc)
  ,  (typeNatKindTyConName , typeNatKindTc)
  ,  (typeSymbolKindTyConName , typeSymbolKindTc)
  ,  (intPrimTyConName , intPrimTc)
  ,  (integerPrimTyConName , integerPrimTc)
  ,  (charPrimTyConName , charPrimTc)
  ,  (stringPrimTyConName , stringPrimTc)
#if !MIN_VERSION_ghc(9,2,0)
  ,  (voidPrimTyConName , voidPrimTc)
#endif
  ,  (wordPrimTyConName , wordPrimTc)
  ,  (int64PrimTyConName , int64PrimTc)
  ,  (word64PrimTyConName , word64PrimTc)
#if MIN_VERSION_ghc(8,8,0)
  ,  (int8PrimTyConName , int8PrimTc)
  ,  (int16PrimTyConName , int16PrimTc)
  ,  (int32PrimTyConName , int32PrimTc)
  ,  (word8PrimTyConName , word8PrimTc)
  ,  (word16PrimTyConName , word16PrimTc)
  ,  (word32PrimTyConName , word32PrimTc)
#endif
  ,  (floatPrimTyConName , floatPrimTc)
  ,  (doublePrimTyConName , doublePrimTc)
  ,  (naturalPrimTyConName , naturalPrimTc)
  ,  (byteArrayPrimTyConName , byteArrayPrimTc)
  ,  (eqPrimTyConName , eqPrimTc)
  ]
