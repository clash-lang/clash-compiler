{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Netlist"
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Netlist (tests) where

import Control.Exception (ErrorCall (..), try)
import Data.Default (def)
import qualified Data.List as List

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Annotations.Primitive (HDL (VHDL))
import Clash.Backend (initBackend)
import Clash.Backend.VHDL (VHDLState)
import Clash.Core.Name (NameSort (User))
import Clash.Core.Term (Term (..))
import Clash.Core.Scoped.NameMap (singletonGlobalEnv)
import Clash.Core.Scoped.Syntax (mkGlobalId)
import Clash.Driver.Types (ClashEnv (..))
import Clash.Netlist (mkFunApp, runNetlistMonad)
import Clash.Netlist.BlackBox (mkBlackBoxContext)
import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types
  ( Declaration (Assignment), DeclarationType (Concurrent), Expr (Identifier)
  , PreserveCase (PreserveCase), SomeBackend (..) )
import Clash.Rewrite.Scoped.Globals (fromClashBindings)
import Clash.Rewrite.Types (RewriteEnv (..))

import Test.Clash.Rewrite (globalId, intFunTy, intId, localId, mkBindingMap)

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
      (_clashEnv env) False (fromClashBindings bs) mempty
      (_typeTranslator env) True be ids "" mempty
      (mkFunApp Concurrent dst xLocal [] [])
  case decls of
    [Assignment lhs _ (Identifier rhs Nothing)] -> do
      assertEqual "Assigned identifier" "result" (Id.toText lhs)
      assertEqual "Assigned expression" "x" (Id.toText rhs)
    _ -> assertFailure
      ("Expected a single assignment, but got " <> show (length decls)
        <> " declarations")

-- | Like 'mkFunAppLocalShadowingGlobal', but for a function passed as an
-- argument to a primitive. 'mkFunInput' determined whether the function was a
-- top entity or a global binder by looking up its unique, so it turned a local
-- variable with the same unique as a global binder into an instance of that
-- binder. A local function can't be turned into a component, so this should
-- be an error instead.
mkFunInputLocalShadowingGlobal :: Assertion
mkFunInputLocalShadowingGlobal = do
  let fGlobal = globalId User "f" 60 (intFunTy 1)
      fLocal = localId User "f" 60 (intFunTy 1)
      a = intId User "a" 61
      r = intId User "r" 63
      res = intId User "res" 62
      -- The global binder is in normal form, so it can become a component
      bs = mkBindingMap [(fGlobal, Lam a (Letrec [(r, Var a)] (Var r)))]
      -- The default rewrite environment of "Test.Clash.Rewrite", for its type
      -- translator and 'ClashEnv'
      env = def :: RewriteEnv
      ids = Id.emptyIdentifierSet False PreserveCase VHDL
      be = SomeBackend (initBackend (envOpts (_clashEnv env)) :: VHDLState)
      compNames = singletonGlobalEnv (mkGlobalId fGlobal) (Id.unsafeMake "f")
  result <- try $
    runNetlistMonad
      (_clashEnv env) False (fromClashBindings bs) mempty
      (_typeTranslator env) True be ids "" compNames
      (mkBlackBoxContext "prim" Concurrent [res] [Left (Var fLocal)])
  case result of
    Left (ErrorCall msg)
      | "Cannot make function input" `List.isInfixOf` msg -> pure ()
      | otherwise -> assertFailure ("Unexpected error:\n" <> msg)
    Right _ -> assertFailure "Expected an error, but got a blackbox context"

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Netlist"
    [ testCase "mkFunApp does not instantiate locals that shadow a global (#1087)"
        mkFunAppLocalShadowingGlobal
    , testCase "mkFunInput does not instantiate locals that shadow a global"
        mkFunInputLocalShadowingGlobal
    ]
