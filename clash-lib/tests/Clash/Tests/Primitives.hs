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
import Clash.Util.Supply (newSupply)

import Clash.Core.Scoped.Convert (convScope, fromClashTerm)
import Clash.Core.Scoped.Syntax
import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Scoped.Types (LocalId (..), compCtxFromList)
import Clash.Netlist.Types (Declaration (Assignment, NetDecl))
import Clash.Primitives.Verification (bindMaybe)

import Test.Clash.Rewrite (intId, intTy, mkBindingMap, runNetlistTest)
import Test.Clash.Scoped (testTypeEnv, withFreeVarsOf)

-- | 'bindMaybe' refers to a local variable by its HDL identifier in the
-- context, and binds other terms to a new signal
bindMaybeTest :: Assertion
bindMaybeTest = do
  sup <- newSupply
  let x = intId C.User "x" 70
  withFreeVarsOf [C.Var x] $ \env -> case fromClashTerm env (C.Var x) of
    xVar@(Var o ty nm) -> do
      let ctx =
            compCtxFromList (convScope env) testTypeEnv sup
              [(LocalId o ty nm, Id.unsafeMake "x_hdl", intTy)]
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
  [ testCase "bindMaybe" bindMaybeTest
  ]
