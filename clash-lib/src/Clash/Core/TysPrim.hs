{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016     , Myrtle Software Ltd,
                     2021-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Builtin types and kinds of scoped Clash Core, and their type constructors.

  Most of them are a single type constructor, which mentions no variables, so
  they are types in any scope: @'Type' n@ for all @n@. Those that bind type
  variables, such as 'undefinedTy', are closed types (@'Type' ''VoidS'@), which
  'sinkClosed' moves into any scope.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Core.TysPrim
  ( -- * Kinds
    liftedTypeKind
  , typeNatKind
  , typeSymbolKind
    -- * Primitive types
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
    -- * Closed types
  , undefinedTy
  , unsafeCoerceTy
    -- * Type constructors
  , tysPrimMap
  , integerIsDc
  , naturalNsDc
    -- * Names of the type constructors
  , liftedTypeKindTyConName
  , typeNatKindTyConName
  , typeSymbolKindTyConName
  , intPrimTyConName
  , integerPrimTyConName
  , charPrimTyConName
  , stringPrimTyConName
  , wordPrimTyConName
  , int64PrimTyConName
  , word64PrimTyConName
  , int8PrimTyConName
  , int16PrimTyConName
  , int32PrimTyConName
  , word8PrimTyConName
  , word16PrimTyConName
  , word32PrimTyConName
  , floatPrimTyConName
  , doublePrimTyConName
  , naturalPrimTyConName
  , byteArrayPrimTyConName
  , eqPrimTyConName
    -- * Type families on naturals
  , typeNatAdd
  , typeNatMul
  , typeNatSub
  ) where

import Data.Text.Extra (showt)
import GHC.Base (ByteArray#, Char#, Double#, Float#, Int#, Int8#, Int16#, Int32#, Int64#,
  Word#, Word8#, Word16#, Word32#, Word64#, Addr#)
import GHC.Builtin.Names
import GHC.Num.Integer (Integer (..))
import GHC.Num.Natural (Natural (..))
import GHC.Types.SrcLoc (wiredInSrcSpan)

import Clash.Core.Basic (ConstTy (..))
import Clash.Core.DataCon (DataCon, DcName, DcStrictness (..), mkDataCon)
import Clash.Core.Name (Name (..), NameSort (User), mkUnsafeSystemName)
import Clash.Core.TyCon
  (AlgTyConRhs (..), TyCon, TyConMap, TyConName, mkAlgTyCon, mkPrimTyCon)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Unique (fromGhcUnique)

import Clash.Core.Sig (DataConSig (..))
import Clash.Core.Subst (withFreshTyBinder)
import Clash.Core.Syntax

-- * Names

liftedTypeKindTyConName, typeNatKindTyConName, typeSymbolKindTyConName :: TyConName
liftedTypeKindTyConName   = mkUnsafeSystemName "Type"
                              (fromGhcUnique liftedTypeKindTyConKey)
typeNatKindTyConName      = naturalPrimTyConName
typeSymbolKindTyConName   = mkUnsafeSystemName "Symbol"
                              (fromGhcUnique typeSymbolKindConNameKey)

intPrimTyConName, integerPrimTyConName, charPrimTyConName, stringPrimTyConName,
  wordPrimTyConName,  int64PrimTyConName, word64PrimTyConName,
  floatPrimTyConName, doublePrimTyConName,
  naturalPrimTyConName, byteArrayPrimTyConName, eqPrimTyConName :: TyConName
intPrimTyConName     = mkUnsafeSystemName (showt ''Int#)
                                (fromGhcUnique intPrimTyConKey)
integerPrimTyConName = mkUnsafeSystemName (showt ''Integer)
                                (fromGhcUnique integerTyConKey)
stringPrimTyConName  = mkUnsafeSystemName (showt ''Addr#)
                        (fromGhcUnique addrPrimTyConKey)
charPrimTyConName    = mkUnsafeSystemName (showt ''Char#)
                                (fromGhcUnique charPrimTyConKey)
wordPrimTyConName    = mkUnsafeSystemName (showt ''Word#)
                                (fromGhcUnique wordPrimTyConKey)
int64PrimTyConName   = mkUnsafeSystemName (showt ''Int64#)
                                (fromGhcUnique int64PrimTyConKey)
word64PrimTyConName  = mkUnsafeSystemName (showt ''Word64#)
                                (fromGhcUnique word64PrimTyConKey)
floatPrimTyConName   = mkUnsafeSystemName (showt ''Float#)
                                (fromGhcUnique floatPrimTyConKey)
doublePrimTyConName  = mkUnsafeSystemName (showt ''Double#)
                                (fromGhcUnique doublePrimTyConKey)
naturalPrimTyConName = mkUnsafeSystemName (showt ''Natural)
                                (fromGhcUnique naturalTyConKey)
byteArrayPrimTyConName = mkUnsafeSystemName (showt ''ByteArray#)
                          (fromGhcUnique byteArrayPrimTyConKey)

eqPrimTyConName = mkUnsafeSystemName "GHC.Prim.~#" (fromGhcUnique eqPrimTyConKey)

-- | GHC's @+@, @*@ and @-@ on type-level naturals, which Clash recognizes by
-- their uniques
typeNatAdd, typeNatMul, typeNatSub :: TyConName
typeNatAdd = Name User "GHC.TypeNats.+" (fromGhcUnique typeNatAddTyFamNameKey) wiredInSrcSpan
typeNatMul = Name User "GHC.TypeNats.*" (fromGhcUnique typeNatMulTyFamNameKey) wiredInSrcSpan
typeNatSub = Name User "GHC.TypeNats.-" (fromGhcUnique typeNatSubTyFamNameKey) wiredInSrcSpan


int8PrimTyConName, int16PrimTyConName, int32PrimTyConName, word8PrimTyConName,
  word16PrimTyConName, word32PrimTyConName :: TyConName
int8PrimTyConName   = mkUnsafeSystemName (showt ''Int8#)
                        (fromGhcUnique int8PrimTyConKey)
int16PrimTyConName  = mkUnsafeSystemName (showt ''Int16#)
                        (fromGhcUnique int16PrimTyConKey)
int32PrimTyConName  = mkUnsafeSystemName (showt ''Int32#)
                        (fromGhcUnique int32PrimTyConKey)
word8PrimTyConName  = mkUnsafeSystemName (showt ''Word8#)
                        (fromGhcUnique word8PrimTyConKey)
word16PrimTyConName = mkUnsafeSystemName (showt ''Word16#)
                        (fromGhcUnique word16PrimTyConKey)
word32PrimTyConName = mkUnsafeSystemName (showt ''Word32#)
                        (fromGhcUnique word32PrimTyConKey)

-- | A builtin type that is a single type constructor, in any scope
tyConTy :: TyConName -> Type n
tyConTy tc = ConstTy (TyCon tc)

-- * Kinds

-- | The kind of lifted types, @Type@
liftedTypeKind :: Kind n
liftedTypeKind = tyConTy liftedTypeKindTyConName

-- | The kind of type-level naturals, @Nat@
typeNatKind :: Kind n
typeNatKind = tyConTy typeNatKindTyConName

-- | The kind of type-level symbols, @Symbol@
typeSymbolKind :: Kind n
typeSymbolKind = tyConTy typeSymbolKindTyConName

-- * Primitive types

intPrimTy, integerPrimTy, charPrimTy, stringPrimTy, wordPrimTy, int64PrimTy,
  word64PrimTy, int8PrimTy, int16PrimTy, int32PrimTy, word8PrimTy,
  word16PrimTy, word32PrimTy, floatPrimTy, doublePrimTy, naturalPrimTy,
  byteArrayPrimTy, eqPrimTy :: Type n
intPrimTy = tyConTy intPrimTyConName
integerPrimTy = tyConTy integerPrimTyConName
charPrimTy = tyConTy charPrimTyConName
stringPrimTy = tyConTy stringPrimTyConName
wordPrimTy = tyConTy wordPrimTyConName
int64PrimTy = tyConTy int64PrimTyConName
word64PrimTy = tyConTy word64PrimTyConName
int8PrimTy = tyConTy int8PrimTyConName
int16PrimTy = tyConTy int16PrimTyConName
int32PrimTy = tyConTy int32PrimTyConName
word8PrimTy = tyConTy word8PrimTyConName
word16PrimTy = tyConTy word16PrimTyConName
word32PrimTy = tyConTy word32PrimTyConName
floatPrimTy = tyConTy floatPrimTyConName
doublePrimTy = tyConTy doublePrimTyConName
naturalPrimTy = tyConTy naturalPrimTyConName
byteArrayPrimTy = tyConTy byteArrayPrimTyConName
eqPrimTy = tyConTy eqPrimTyConName

-- * Closed types

-- | The type @forall a. a@
undefinedTy :: Type 'VoidS
undefinedTy =
  withFreshTyBinder emptyScope (mkUnsafeSystemName "a" 0) liftedTypeKind $ \_ a ->
    ForAllTy a (tyBinderVar a)

-- | The type @forall a. forall b. a -> b@
unsafeCoerceTy :: Type 'VoidS
unsafeCoerceTy =
  withFreshTyBinder emptyScope (mkUnsafeSystemName "a" 0) liftedTypeKind $ \sc1 a ->
    withFreshTyBinder sc1 (mkUnsafeSystemName "b" 1) liftedTypeKind $ \_ b ->
      ForAllTy a (ForAllTy b (funTy (sink (tyBinderVar a)) (tyBinderVar b)))

-- | A function type
funTy :: Type n -> Type n -> Type n
funTy t1 = AppTy (AppTy (ConstTy Arrow) t1)

-- * Type constructors

-- | The builtin type constructors
tysPrimMap :: TyConMap
tysPrimMap = UniqMap.fromList
  [ (liftedTypeKindTyConName, liftedTypeKindTc)
  , (typeNatKindTyConName, typeNatKindTc)
  , (typeSymbolKindTyConName, typeSymbolKindTc)
  , (intPrimTyConName, intPrimTc)
  , (integerPrimTyConName, integerPrimTc)
  , (charPrimTyConName, charPrimTc)
  , (stringPrimTyConName, stringPrimTc)
  , (wordPrimTyConName, wordPrimTc)
  , (int64PrimTyConName, int64PrimTc)
  , (word64PrimTyConName, word64PrimTc)
  , (int8PrimTyConName, int8PrimTc)
  , (int16PrimTyConName, int16PrimTc)
  , (int32PrimTyConName, int32PrimTc)
  , (word8PrimTyConName, word8PrimTc)
  , (word16PrimTyConName, word16PrimTc)
  , (word32PrimTyConName, word32PrimTc)
  , (floatPrimTyConName, floatPrimTc)
  , (doublePrimTyConName, doublePrimTc)
  , (naturalPrimTyConName, naturalPrimTc)
  , (byteArrayPrimTyConName, byteArrayPrimTc)
  , (eqPrimTyConName, eqPrimTc)
  ]

-- | A primitive type constructor of kind 'liftedTypeKind'
liftedPrimTc :: TyConName -> TyCon
liftedPrimTc nm = mkPrimTyCon nm liftedTypeKind 0

liftedTypeKindTc, typeNatKindTc, typeSymbolKindTc, intPrimTc, integerPrimTc,
  charPrimTc, stringPrimTc, wordPrimTc, int64PrimTc, word64PrimTc, int8PrimTc,
  int16PrimTc, int32PrimTc, word8PrimTc, word16PrimTc, word32PrimTc,
  floatPrimTc, doublePrimTc, naturalPrimTc, byteArrayPrimTc, eqPrimTc :: TyCon
liftedTypeKindTc = liftedPrimTc liftedTypeKindTyConName
typeNatKindTc = liftedPrimTc typeNatKindTyConName
typeSymbolKindTc = liftedPrimTc typeSymbolKindTyConName
intPrimTc = liftedPrimTc intPrimTyConName
charPrimTc = liftedPrimTc charPrimTyConName
stringPrimTc = liftedPrimTc stringPrimTyConName
wordPrimTc = liftedPrimTc wordPrimTyConName
int64PrimTc = liftedPrimTc int64PrimTyConName
word64PrimTc = liftedPrimTc word64PrimTyConName
int8PrimTc = liftedPrimTc int8PrimTyConName
int16PrimTc = liftedPrimTc int16PrimTyConName
int32PrimTc = liftedPrimTc int32PrimTyConName
word8PrimTc = liftedPrimTc word8PrimTyConName
word16PrimTc = liftedPrimTc word16PrimTyConName
word32PrimTc = liftedPrimTc word32PrimTyConName
floatPrimTc = liftedPrimTc floatPrimTyConName
doublePrimTc = liftedPrimTc doublePrimTyConName
byteArrayPrimTc = liftedPrimTc byteArrayPrimTyConName

-- While GHC might have dropped Integer and Natural literals, in Clash it is
-- still nice to have them around. However, Integer and Natural are also no
-- longer primitive types in GHC, but we still want to give the Integer and
-- Natural type to our Integer and Natural literals.
--
-- So instead of recording the primitive types, we record the algebraic types,
-- i.e. the complete data type for Integer and Natural, data constructors and
-- all.
integerPrimTc =
  mkAlgTyCon integerPrimTyConName liftedTypeKind 0 (DataTyCon [integerIsDc, ipDc, inDc]) False
 where
  ipDc = boxDc (mkUnsafeSystemName (showt 'IP) (fromGhcUnique integerIPDataConKey))
           2 integerPrimTy byteArrayPrimTy
  inDc = boxDc (mkUnsafeSystemName (showt 'IN) (fromGhcUnique integerINDataConKey))
           3 integerPrimTy byteArrayPrimTy

naturalPrimTc =
  mkAlgTyCon naturalPrimTyConName liftedTypeKind 0 (DataTyCon [naturalNsDc, nbDc]) False
 where
  nbDc = boxDc (mkUnsafeSystemName (showt 'NB) (fromGhcUnique naturalNBDataConKey))
           2 naturalPrimTy byteArrayPrimTy

-- | The data constructors of 'Integer' and 'Natural' that hold a machine word
integerIsDc, naturalNsDc :: DataCon
integerIsDc =
  boxDc (mkUnsafeSystemName (showt 'IS) (fromGhcUnique integerISDataConKey))
    1 integerPrimTy intPrimTy
naturalNsDc =
  boxDc (mkUnsafeSystemName (showt 'NS) (fromGhcUnique naturalNSDataConKey))
    1 naturalPrimTy wordPrimTy

-- | A data constructor with the given name and tag, of a type with no type
-- variables, with one strict field of the given type
boxDc :: DcName -> Int -> Type 'VoidS -> Type 'VoidS -> DataCon
boxDc nm tag resTy argTy =
  mkDataCon nm tag (funTy argTy resTy) (DataConSig TNil TNil [argTy]) [Strict] []

-- | The kind @forall (a :: Type). forall (b :: Type). a -> b -> Type@. The
-- "real" kind of this in GHC has a codomain of @TYPE ('TupleRep '[])@ instead
-- of the @TYPE 'LiftedRep@ used here.
eqPrimTc = mkPrimTyCon eqPrimTyConName kind 4
 where
  kind =
    withFreshTyBinder emptyScope (mkUnsafeSystemName "a" 0) liftedTypeKind $ \sc1 a ->
      withFreshTyBinder sc1 (mkUnsafeSystemName "b" 1) liftedTypeKind $ \_ b ->
        ForAllTy a (ForAllTy b
          (funTy (sink (tyBinderVar a)) (funTy (tyBinderVar b) liftedTypeKind)))
