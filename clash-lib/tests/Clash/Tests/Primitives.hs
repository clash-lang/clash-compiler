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
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C

import Clash.Core.Scoped.Convert (convScope, fromClashTerm, fromClashType)
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

import Test.Clash.Rewrite
  (addTy, intId, mkBindingMap, numTy, runNetlistTest, tyVar)
import Test.Clash.Scoped (testTypeEnv, withFreeVarsOf)

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
    [ withFreeVarsOf [C.TyApp (C.Literal (C.IntLiteral 0)) resTy] $ \env -> do
        let ctx = compCtxFromList (convScope env) testTypeEnv []
            conv = either (Left . fromClashTerm env) (Right . fromClashType env)
            resTys = [fromClashType env resTy]
        plurality <- runNetlistTest (mkBindingMap []) $
          getFunctionPlurality ctx (bbfPrim f) (map conv args) resTys i
        assertEqual nm expected plurality
    | (nm, f, args, i, expected) <- cases ]
 where
  a = tyVar C.User "a" 70
  resTy = C.VarTy a
  cases =
    [ -- fold :: (a -> a -> a) -> Vec (n + 1) a -> a, with n + 1 = 8
      ( "fold", ScopedBBF foldBBF
      , [Right (addTy (numTy 6) (numTy 1)), Right resTy], 0, 7 )
      -- iterateI :: KnownNat n => (a -> a) -> a -> Vec n a, with n = 5
    , ( "iterateI", ScopedBBF iterateBBF
      , [Right (addTy (numTy 4) (numTy 1)), Right resTy], 1, 4 )
    ]

-- | 'bindMaybe' refers to a local variable by its HDL identifier in the
-- context, and binds other terms to a new signal
bindMaybeTest :: Assertion
bindMaybeTest = do
  let x = intId C.User "x" 70
  withFreeVarsOf [C.Var x] $ \env -> case fromClashTerm env (C.Var x) of
    xVar@(Var o ty nm) -> do
      let ctx =
            compCtxFromList (convScope env) testTypeEnv
              [(LocalId o ty nm, Id.unsafeMake "x_hdl")]
      (i0, decls0) <- runNetlistTest (mkBindingMap []) (bindMaybe ctx (Just "p") xVar)
      assertEqual "identifier of a variable" "x_hdl" (Id.toText i0)
      assertEqual "declarations of a variable" 0 (length decls0)
      (i1, decls1) <-
        runNetlistTest (mkBindingMap []) (bindMaybe ctx (Just "p") (Literal (C.IntLiteral 3)))
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
