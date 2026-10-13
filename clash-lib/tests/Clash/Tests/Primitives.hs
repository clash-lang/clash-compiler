{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for the blackbox functions of "Clash.Primitives" on scoped Clash Core
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Primitives (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Literal as C
import qualified Clash.Core.Basic as C (WorkInfo (WorkVariable))
import qualified Clash.Core.Basic as C (ConstTy (TyCon), LitTy (NumTy))

import Clash.Core.Scoped.Builder (TyB, appTyB, constTyB, litTyB)
import Clash.Core.Scoped.NameMap (emptyGlobalEnv)
import Clash.Core.Scoped.Syntax
import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Scoped.Types
  ( LocalId (..), SomeBlackBoxFunction (..), compCtxFromList )
import Clash.Netlist.Types (Declaration (Assignment, NetDecl))
import Clash.Primitives.Types
  ( BlackBoxFunctionName (..), CompiledPrimitive, Primitive (BlackBoxHaskell)
  , UsedArguments (UsedArguments) )
import Clash.Primitives.Scoped.Util (getFunctionPlurality)
import Clash.Primitives.Sized.Vector (foldBBF, iterateBBF)
import Clash.Primitives.Verification (bindMaybe)

import Test.Clash.Rewrite (addTyConName, runScopedNetlistTest)
import Test.Clash.Scoped
  ( TestType (..), builtType, termIn, testScope, testTypeEnv, tyVarD, typeIn, varD
  , withTestScope )

-- | A primitive with the given blackbox function
bbfPrim :: SomeBlackBoxFunction -> CompiledPrimitive
bbfPrim f =
  BlackBoxHaskell "prim" C.WorkVariable (UsedArguments []) False
    (BlackBoxFunctionName ["M"] "f") (0, f)

-- | The function plurality that the blackbox functions of @fold@ and
-- @iterateI@ give, through 'getFunctionPlurality'. The arguments are in a
-- scope with a type variable, and the length of the vector is a type family
-- application, so the blackbox functions have to look through it in that
-- scope.
functionPlurality :: Assertion
functionPlurality = do
  sequence_
    [ withTestScope [tyVarD "a"] $ \ts -> do
        let ctx = compCtxFromList (testScope ts) testTypeEnv []
        plurality <- runScopedNetlistTest emptyGlobalEnv $
          getFunctionPlurality ctx (bbfPrim f) (map (Right . typeIn ts) args) [typeIn ts "a"] i
        assertEqual nm expected plurality
    | (nm, f, args, i, expected) <- cases ]
 where
  -- @m + n@
  addB :: Integer -> Integer -> TestType
  addB m n =
    builtType (appTyB (appTyB (constTyB (C.TyCon addTyConName)) (litB m)) (litB n))

  litB :: Integer -> TyB
  litB = litTyB . C.NumTy

  cases =
    [ -- fold :: (a -> a -> a) -> Vec (n + 1) a -> a, with n + 1 = 8
      ( "fold", ScopedBBF foldBBF, [addB 6 1, "a"], 0, 7 )
      -- iterateI :: KnownNat n => (a -> a) -> a -> Vec n a, with n = 5
    , ( "iterateI", ScopedBBF iterateBBF, [addB 4 1, "a"], 1, 4 )
    ]

-- | 'bindMaybe' refers to a local variable by its HDL identifier in the
-- context, and binds other terms to a new signal
bindMaybeTest :: Assertion
bindMaybeTest =
  withTestScope [varD "x" "Int"] $ \ts -> case termIn ts "x" of
    xVar@(Var o ty nm) -> do
      let ctx =
            compCtxFromList (testScope ts) testTypeEnv
              [(LocalId o ty nm, Id.unsafeMake "x_hdl")]
      (i0, decls0) <- runScopedNetlistTest emptyGlobalEnv (bindMaybe ctx (Just "p") xVar)
      assertEqual "identifier of a variable" "x_hdl" (Id.toText i0)
      assertEqual "declarations of a variable" 0 (length decls0)
      (i1, decls1) <-
        runScopedNetlistTest emptyGlobalEnv (bindMaybe ctx (Just "p") (Literal (C.IntLiteral 3)))
      assertEqual "identifier of a literal" "p" (Id.toText i1)
      case decls1 of
        [NetDecl _ d _, Assignment l _ _] -> do
          assertEqual "declared" "p" (Id.toText d)
          assertEqual "assigned" "p" (Id.toText l)
        _ -> assertFailure ("Unexpected declarations: " <> show decls1)
    tm -> assertFailure ("Expected a variable, got: " <> show tm)

tests :: TestTree
tests = testGroup "Clash.Tests.Primitives"
  [ testCase "getFunctionPlurality of fold and iterateI" functionPlurality
  , testCase "bindMaybe" bindMaybeTest
  ]
