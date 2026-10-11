{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for the builtin types of scoped Clash Core: they agree with those of
  "Clash.Core.TysPrim".
-}

{-# LANGUAGE DataKinds #-}

module Clash.Tests.Core.Scoped.TysPrim (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Type as C
import qualified Clash.Core.TysPrim as C
import qualified Clash.Core.Util as C

import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.TysPrim

import Test.Clash.Scoped (assertScopedTyAlphaEq)

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.TysPrim"
  [ testCase "builtin types agree with Clash.Core.TysPrim" $
      mapM_ (uncurry assertScopedTyAlphaEq) builtins
  , testCase "undefinedTy" $ assertScopedTyAlphaEq C.undefinedTy undefinedTy
  , testCase "unsafeCoerceTy" $ assertScopedTyAlphaEq C.unsafeCoerceTy unsafeCoerceTy
  ]

builtins :: [(C.Type, Type 'VoidS)]
builtins =
  [ (C.liftedTypeKind, liftedTypeKind)
  , (C.typeNatKind, typeNatKind)
  , (C.typeSymbolKind, typeSymbolKind)
  , (C.intPrimTy, intPrimTy)
  , (C.integerPrimTy, integerPrimTy)
  , (C.charPrimTy, charPrimTy)
  , (C.stringPrimTy, stringPrimTy)
  , (C.wordPrimTy, wordPrimTy)
  , (C.int64PrimTy, int64PrimTy)
  , (C.word64PrimTy, word64PrimTy)
  , (C.int8PrimTy, int8PrimTy)
  , (C.int16PrimTy, int16PrimTy)
  , (C.int32PrimTy, int32PrimTy)
  , (C.word8PrimTy, word8PrimTy)
  , (C.word16PrimTy, word16PrimTy)
  , (C.word32PrimTy, word32PrimTy)
  , (C.floatPrimTy, floatPrimTy)
  , (C.doublePrimTy, doublePrimTy)
  , (C.naturalPrimTy, naturalPrimTy)
  , (C.byteArrayPrimTy, byteArrayPrimTy)
  , (C.eqPrimTy, eqPrimTy)
  ]
