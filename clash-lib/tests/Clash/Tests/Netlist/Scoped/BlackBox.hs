{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Netlist.Scoped.BlackBox"
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Netlist.Scoped.BlackBox (tests) where

import Control.Exception (ErrorCall (..), try)
import Control.Lens ((.=))
import qualified Data.IntMap as IntMap
import qualified Data.List as List

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (NameSort (User))
import qualified Clash.Core.Term as C
import Clash.Netlist.Scoped.BlackBox (mkBlackBoxContext)
import Clash.Netlist.Scoped.Types (LocalId (..))
import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types
  ( BlackBoxContext (..), Declaration (Assignment, InstDecl), DeclarationType (Concurrent)
  , Expr (Identifier), componentNames )
import Clash.Core.Scoped.NameMap (singletonGlobalEnv)
import Clash.Core.Scoped.Syntax (Term (..), globalIdLegacy)
import Clash.Core.Scoped.Convert (mkGlobalIdFromClash)

import Test.Clash.Rewrite (globalId, intFunTy, intId, localId, mkBindingMap, runNetlistTest)
import Test.Clash.Scoped (withCompCtxOf)

-- | The declarations of the first function argument of a blackbox context
functionDecls :: BlackBoxContext -> [Declaration]
functionDecls bbCtx = case IntMap.lookup 0 (bbFunctions bbCtx) of
  Just ((Right (_, decls), _, _, _, _, _) : _) -> decls
  _ -> error "Expected the declarations of a function argument"

-- | Clash Core's 'mkFunInput' determined whether a function argument was a
-- top entity or a global binder by looking up its unique, so it turned a
-- local variable with the same unique as a global binder into an instance of
-- that binder. On scoped Core a local function is a 'Var', which can't be
-- turned into a component, so this is an error.
mkFunInputLocalShadowingGlobal :: Assertion
mkFunInputLocalShadowingGlobal = do
  let fGlobal = globalId User "f" 60 (intFunTy 1)
      fLocal = localId User "f" 60 (intFunTy 1)
      a = intId User "a" 61
      r = intId User "r" 63
      res = intId User "res" 62
      -- The global binder is in normal form, so it can become a component
      bs = mkBindingMap [(fGlobal, C.Lam a (C.Letrec [(r, C.Var a)] (C.Var r)))]
      compNames = singletonGlobalEnv (mkGlobalIdFromClash fGlobal) (Id.unsafeMake "f")
  result <- try $ withCompCtxOf [C.Var fLocal, C.Var res] $ \ctx tms -> case tms of
    [f, Var o ty n] ->
      runNetlistTest bs $ do
        componentNames .= compNames
        mkBlackBoxContext ctx "prim" Concurrent [LocalId o ty n] [Left f]
    _ -> error "unexpected terms"
  case result of
    Left (ErrorCall msg)
      | "Cannot make function input" `List.isInfixOf` msg -> pure ()
      | otherwise -> assertFailure ("Unexpected error:\n" <> msg)
    Right _ -> assertFailure "Expected an error, but got a blackbox context"

-- | A global function argument becomes an instance of its component
mkFunInputGlobal :: Assertion
mkFunInputGlobal = do
  let fGlobal = globalId User "f" 60 (intFunTy 1)
      a = intId User "a" 61
      r = intId User "r" 63
      res = intId User "res" 62
      bs = mkBindingMap [(fGlobal, C.Lam a (C.Letrec [(r, C.Var a)] (C.Var r)))]
      compNames = singletonGlobalEnv (mkGlobalIdFromClash fGlobal) (Id.unsafeMake "f")
      g = mkGlobalIdFromClash fGlobal
  bbCtx <- withCompCtxOf [C.Var res] $ \ctx tms -> case tms of
    [Var o ty n] ->
      fmap fst $ runNetlistTest bs $ do
        componentNames .= compNames
        mkBlackBoxContext ctx "prim" Concurrent [LocalId o ty n] [Left (GlobalVar g)]
    _ -> error "unexpected terms"
  case functionDecls bbCtx of
    [InstDecl _ _ _ compName _ _ _] ->
      assertEqual "Instantiated component" "f" (Id.toText compName)
    decls -> assertFailure ("Expected an instance of f, but got: " <> show decls
                              <> " for " <> show (globalIdLegacy g))

-- | The variables of the lambdas of a function argument get the HDL
-- identifiers of the arguments of the function in the template, and those of
-- its let-bindings unique ones
mkFunInputLambda :: Assertion
mkFunInputLambda = do
  let a = intId User "a" 61
      r = intId User "r" 63
      res = intId User "res" 62
      fun = C.Lam a (C.Letrec [(r, C.Var a)] (C.Var r))
  bbCtx <- withCompCtxOf [fun, C.Var res] $ \ctx tms -> case tms of
    [f, Var o ty n] ->
      fst <$> runNetlistTest (mkBindingMap [])
        (mkBlackBoxContext ctx "prim" Concurrent [LocalId o ty n] [Left f])
    _ -> error "unexpected terms"
  case functionDecls bbCtx of
    Assignment res1 _ (Identifier r1 Nothing) : rest -> do
      assertEqual "Result" "~RESULT" (Id.toText res1)
      assertEqual "Let-binder" "r" (Id.toText r1)
      assertBool ("Assignment of the argument to r in: " <> show rest)
        (any (isAssignment "r" "~ARGN[1][0]") rest)
    decls -> assertFailure ("Unexpected declarations: " <> show decls)
 where
  isAssignment dst src = \case
    Assignment d _ (Identifier s Nothing) -> Id.toText d == dst && Id.toText s == src
    _ -> False

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Netlist.Scoped.BlackBox"
    [ testCase "mkFunInput does not instantiate locals that shadow a global"
        mkFunInputLocalShadowingGlobal
    , testCase "mkFunInput instantiates a global function" mkFunInputGlobal
    , testCase "mkFunInput names the variables of a lambda" mkFunInputLambda
    ]
