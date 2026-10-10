{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Netlist"
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Netlist (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Annotations.Primitive (HDL (VHDL))
import Clash.Backend (initBackend)
import Clash.Backend.VHDL (VHDLState)
import Clash.Core.Name (NameSort (User))
import Clash.Core.Term (Term (..))
import Clash.Driver.Types (ClashEnv (..))
import Clash.Netlist (mkFunApp, runNetlistMonad)
import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types
  ( Declaration (Assignment), DeclarationType (Concurrent), Expr (Identifier)
  , PreserveCase (PreserveCase), SomeBackend (..) )
import Clash.Rewrite.Types (RewriteEnv (..))

import Test.Clash.Rewrite (globalId, intFunTy, intId, mkBindingMap)

-- | When 'mkFunApp' encountered a function application, it determined whether
-- the function was a top entity or a normalized global binder by looking up its
-- unique in the top entity annotations or the global bindings. A local variable
-- with the same unique as a global binder was therefore turned into an
-- instance of that binder.
--
-- This applies @x@, a local variable without arguments that has the same unique
-- as the global binder @x@, which should become a plain assignment.
--
-- https://github.com/clash-lang/clash-compiler/pull/1087
mkFunAppLocalShadowingGlobal :: Assertion
mkFunAppLocalShadowingGlobal = do
  let xGlobal = globalId User "x" 60 (intFunTy 1)
      xLocal = intId User "x" 60
      a = intId User "a" 61
      bs = mkBindingMap [(xGlobal, Lam a (Var a))]
      -- The default rewrite environment of "Test.Clash.Rewrite", for its type
      -- translator and 'ClashEnv'
      env = def :: RewriteEnv
      ids = Id.emptyIdentifierSet False PreserveCase VHDL
      be = SomeBackend (initBackend (envOpts (_clashEnv env)) :: VHDLState)
      dst = Id.unsafeMake "result"
  (decls, _) <-
    runNetlistMonad
      (_clashEnv env) (error "evaluator") False bs mempty
      (_typeTranslator env) True be ids "" mempty
      (mkFunApp Concurrent dst xLocal [] [])
  case decls of
    [Assignment lhs _ (Identifier rhs Nothing)] -> do
      assertEqual "Assigned identifier" "result" (Id.toText lhs)
      assertEqual "Assigned expression" "x" (Id.toText rhs)
    _ -> assertFailure
      ("Expected a single assignment, but got " <> show (length decls)
        <> " declarations")

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Netlist"
    [ testCase "mkFunApp does not instantiate locals that shadow a global (#1087)"
        mkFunAppLocalShadowingGlobal
    ]
