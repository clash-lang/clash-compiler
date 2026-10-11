{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.HasType"
-}

module Clash.Tests.Core.HasType (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.HasType (inferCoreKindOf)
import Clash.Core.Name (NameSort (User))
import Clash.Core.Type (Type (VarTy))
import Clash.Core.TysPrim (liftedTypeKind)

import Test.Clash.Rewrite
  (assertAlphaEq, intTy, maybeTy, maybeTyConMap, tyVar)

-- | With the @debug@ flag, 'Clash.Core.HasType.piResultTys' checks that the
-- types it drops the arrows of a function type with match the argument types
-- of the function type. 'inferCoreKindOf' used it to apply the kind of a type
-- constructor to its arguments, such as @Type -> Type@ of @Maybe@ to @Int@. The
-- check then compared @Type@ against @Int@, so 'inferCoreKindOf' errored.
inferKindOfTyConApp :: Assertion
inferKindOfTyConApp =
  assertAlphaEq liftedTypeKind (inferCoreKindOf maybeTyConMap (maybeTy intTy))

-- | As 'inferKindOfTyConApp', but applied to a type variable
inferKindOfTyConAppVar :: Assertion
inferKindOfTyConAppVar =
  assertAlphaEq liftedTypeKind (inferCoreKindOf maybeTyConMap (maybeTy (VarTy a)))
 where
  a = tyVar User "a" 1

tests :: TestTree
tests = testGroup "Clash.Core.HasType"
  [ testGroup "inferCoreKindOf"
      [ testCase "type constructor applied to a type" inferKindOfTyConApp
      , testCase "type constructor applied to a type variable" inferKindOfTyConAppVar
      ]
  ]
