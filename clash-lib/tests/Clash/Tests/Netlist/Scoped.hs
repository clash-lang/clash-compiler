{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Netlist.Scoped"
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Netlist.Scoped (tests) where

import Control.Lens ((.=))

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Var as C (Id)
import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types
  ( Declaration (Assignment, InstDecl), DeclarationType (Concurrent), EntityOrComponent (Entity)
  , Expr (Identifier), Identifier, NetlistMonad, PortDirection (..), PortMap (NamedPortMap)
  , componentNames )

import Clash.Core.Scoped.Convert (fromClashTypeClosed)
import Clash.Core.Scoped.NameMap (singletonGlobalEnv)
import Clash.Core.Scoped.Subst (withFreshIdBinder)
import Clash.Core.Scoped.Syntax
import Clash.Netlist.Scoped (mkFunApp)
import Clash.Netlist.Scoped.Types (CompCtx, LocalId, binderLocalId, compCtxFromList, localIdVar)

import Test.Clash.Rewrite (globalId, intFunTy, intId, intTy, mkBindingMap, runNetlistTest)
import Test.Clash.Scoped (testTypeEnv)

-- | Run a Netlist action on a local variable @y@ of type @Int@, with HDL
-- identifier @y@, in a design with the given global bindings and component
-- names
withLocal
  :: [(C.Id, C.Term)]
  -> [(C.Id, Identifier)]
  -> (forall n. Distinct n => CompCtx n -> LocalId n -> NetlistMonad a)
  -> IO a
withLocal bs compNames act =
  runNetlistTest (mkBindingMap bs) $ do
    componentNames .= mconcat [ singletonGlobalEnv (mkGlobalId g) i | (g, i) <- compNames ]
    withFreshIdBinder emptyScope (C.mkUnsafeSystemName "y" 0)
      (fromClashTypeClosed emptyScope intTy) $ \sc b ->
        let y = binderLocalId b
            ctx = compCtxFromList sc testTypeEnv [(y, Id.unsafeMake "y")]
        in act ctx y

-- | A local variable can't be mistaken for a global binder (#1087), even one
-- with the same name: applying it without arguments assigns it.
mkFunAppLocal :: Assertion
mkFunAppLocal = do
  let yGlobal = globalId C.User "y" 60 (intFunTy 1)
      a = intId C.User "a" 61
  decls <- withLocal [(yGlobal, C.Lam a (C.Var a))] [] $ \ctx y ->
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
  let f = globalId C.User "f" 60 (intFunTy 1)
      a = intId C.User "a" 61
      r = intId C.User "r" 62
  decls <- withLocal [(f, C.Lam a (C.Letrec [(r, C.Var a)] (C.Var r)))] [(f, Id.unsafeMake "f")] $
    \ctx y ->
      mkFunApp ctx Concurrent (Id.unsafeMake "result") (Right (mkGlobalId f)) [localIdVar y] []
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
