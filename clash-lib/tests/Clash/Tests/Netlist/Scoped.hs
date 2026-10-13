{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Netlist"
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Netlist.Scoped (tests) where

import Control.Lens ((.=))

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types
  (Declaration (Assignment, InstDecl), DeclarationType (Concurrent), EntityOrComponent (Entity)
  , Expr (Identifier), Identifier, NetlistMonad, PortDirection (..), PortMap (NamedPortMap)
  , componentNames, CompCtx, LocalId (..), localIdVar)

import Clash.Core.NameMap (singletonGlobalEnv)
import Clash.Core.Syntax
import Clash.Netlist (mkFunApp)

import Test.Clash.Rewrite (runNetlistTest)
import Test.Clash.Scoped
  (TestTerm, atRaw, mkGlobalBindings, termIn, testCompCtx, testGlobalId, varD, withTestScope)

-- | Run a Netlist action on a local variable @y@ of type @Int@, with HDL
-- identifier @y@, in a design with the given global bindings and component
-- names. The raw name of @y@ is 60, the unique of the globals of the tests.
withLocal
  :: [(GlobalId, TestTerm)]
  -> [(GlobalId, Identifier)]
  -> (forall n. Distinct n => CompCtx n -> LocalId n -> NetlistMonad a)
  -> IO a
withLocal bs compNames act =
  withTestScope [atRaw 60 (varD "y" "Int")] $ \ts -> case termIn ts "y" of
    Var o ty n ->
      runNetlistTest (mkGlobalBindings bs) $ do
        componentNames .= mconcat [ singletonGlobalEnv g i | (g, i) <- compNames ]
        act (testCompCtx ts) (LocalId o ty n)
    _ -> error "withLocal: not a variable"

-- | A local variable can't be mistaken for a global binder (#1087), even one
-- whose unique is the raw name of the local: applying it without arguments
-- assigns it.
mkFunAppLocal :: Assertion
mkFunAppLocal = do
  let yGlobal = testGlobalId "y_G60" "Int -> Int"
  decls <- withLocal [(yGlobal, "\\(a :: Int) -> a")] [] $ \ctx y ->
    mkFunApp ctx Concurrent (Id.unsafeMake "result") (Left y) [] []
  case decls of
    [Assignment lhs _ (Identifier rhs Nothing)] -> do
      assertEqual "Assigned identifier" "result" (Id.toText lhs)
      assertEqual "Assigned expression" "y" (Id.toText rhs)
    _ -> assertFailure
      ("Expected a single assignment, but got " <> show (length decls)
        <> " declarations")

-- | Applying a global binder instantiates its component, with the argument
-- connected to its input port.
mkFunAppGlobal :: Assertion
mkFunAppGlobal = do
  let f = testGlobalId "f_G60" "Int -> Int"
  decls <- withLocal [(f, "\\(a :: Int) -> let { (r :: Int) = a } in r")] [(f, Id.unsafeMake "f")] $
    \ctx y ->
      mkFunApp ctx Concurrent (Id.unsafeMake "result") (Right f) [localIdVar y] []
  case decls of
    [InstDecl Entity Nothing [] compName _ [] (NamedPortMap ports)] -> do
      assertEqual "Component" "f" (Id.toText compName)
      assertEqual "Port directions" [Out, In] [ dir | (_, dir, _, _) <- ports ]
      assertEqual "Connected expressions" ["result", "y"]
        [ Id.toText i | (_, _, _, Identifier i Nothing) <- ports ]
    _ -> assertFailure
      ("Expected a single instance, but got " <> show (length decls)
        <> " declarations")

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Netlist.Scoped"
    [ testCase "mkFunApp does not instantiate a local with the name of a global (#1087)" mkFunAppLocal
    , testCase "mkFunApp instantiates the component of a global binder" mkFunAppGlobal
    ]
