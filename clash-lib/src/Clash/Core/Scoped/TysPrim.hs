{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Builtin types and kinds of scoped Clash Core. See "Clash.Core.TysPrim".

  Most of them are a single type constructor, which mentions no variables, so
  they are types in any scope: @'Type' n@ for all @n@. Those that bind type
  variables, such as 'undefinedTy', are closed types (@'Type' ''VoidS'@), which
  'sinkClosed' moves into any scope.

  The type constructors themselves are the 'Clash.Core.TyCon.TyCon's of
  'Clash.Core.TysPrim.tysPrimMap'.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Core.Scoped.TysPrim
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
  ) where

import GHC.Stack (HasCallStack)

import qualified Clash.Core.Name as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.TysPrim as C

import Clash.Core.Scoped.Subst (withFreshTyBinder)
import Clash.Core.Scoped.Syntax

-- | A builtin type that is a single type constructor, in any scope
fromConstTy :: HasCallStack => C.Type -> Type n
fromConstTy = \case
  C.ConstTy c -> ConstTy c
  ty -> error ("fromConstTy: not a type constructor: " <> show ty)

-- * Kinds

-- | See 'C.liftedTypeKind'
liftedTypeKind :: Kind n
liftedTypeKind = fromConstTy C.liftedTypeKind

-- | See 'C.typeNatKind'
typeNatKind :: Kind n
typeNatKind = fromConstTy C.typeNatKind

-- | See 'C.typeSymbolKind'
typeSymbolKind :: Kind n
typeSymbolKind = fromConstTy C.typeSymbolKind

-- * Primitive types

intPrimTy, integerPrimTy, charPrimTy, stringPrimTy, wordPrimTy, int64PrimTy,
  word64PrimTy, int8PrimTy, int16PrimTy, int32PrimTy, word8PrimTy,
  word16PrimTy, word32PrimTy, floatPrimTy, doublePrimTy, naturalPrimTy,
  byteArrayPrimTy, eqPrimTy :: Type n
intPrimTy = fromConstTy C.intPrimTy
integerPrimTy = fromConstTy C.integerPrimTy
charPrimTy = fromConstTy C.charPrimTy
stringPrimTy = fromConstTy C.stringPrimTy
wordPrimTy = fromConstTy C.wordPrimTy
int64PrimTy = fromConstTy C.int64PrimTy
word64PrimTy = fromConstTy C.word64PrimTy
int8PrimTy = fromConstTy C.int8PrimTy
int16PrimTy = fromConstTy C.int16PrimTy
int32PrimTy = fromConstTy C.int32PrimTy
word8PrimTy = fromConstTy C.word8PrimTy
word16PrimTy = fromConstTy C.word16PrimTy
word32PrimTy = fromConstTy C.word32PrimTy
floatPrimTy = fromConstTy C.floatPrimTy
doublePrimTy = fromConstTy C.doublePrimTy
naturalPrimTy = fromConstTy C.naturalPrimTy
byteArrayPrimTy = fromConstTy C.byteArrayPrimTy
eqPrimTy = fromConstTy C.eqPrimTy

-- * Closed types

-- | The type @forall a. a@. See 'Clash.Core.Util.undefinedTy'.
undefinedTy :: Type 'VoidS
undefinedTy =
  withFreshTyBinder emptyScope (C.mkUnsafeSystemName "a" 0) liftedTypeKind $ \_ a ->
    ForAllTy a (tyBinderVar a)

-- | The type @forall a. forall b. a -> b@. See
-- 'Clash.Core.Util.unsafeCoerceTy'.
unsafeCoerceTy :: Type 'VoidS
unsafeCoerceTy =
  withFreshTyBinder emptyScope (C.mkUnsafeSystemName "a" 0) liftedTypeKind $ \sc1 a ->
    withFreshTyBinder sc1 (C.mkUnsafeSystemName "b" 1) liftedTypeKind $ \_ b ->
      ForAllTy a (ForAllTy b (funTy (sink (tyBinderVar a)) (tyBinderVar b)))
 where
  funTy t1 = AppTy (AppTy (ConstTy C.Arrow) t1)
