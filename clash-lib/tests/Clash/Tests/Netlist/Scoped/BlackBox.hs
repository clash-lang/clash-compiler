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

import Clash.Netlist.Scoped.BlackBox (mkBlackBoxContext)
import Clash.Netlist.Scoped.Types (LocalId (..))
import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types
  ( BlackBoxContext (..), Declaration (Assignment, InstDecl), DeclarationType (Concurrent)
  , Expr (Identifier), componentNames )
import Clash.Core.Scoped.NameMap (emptyGlobalEnv, singletonGlobalEnv)
import Clash.Core.Scoped.Syntax (GlobalId (..), Term (..))

import Test.Clash.Rewrite (runScopedNetlistTest)
import Test.Clash.Scoped
  ( TestTerm, atRaw, mkGlobalBindings, termIn, testCompCtx, testGlobalId, varD, withTerms
  , withTestScope )

-- | A global function @f@ in normal form, which can become a component, and
-- its component name
fGlobal :: GlobalId
fGlobal = testGlobalId "f_G60" "Int -> Int"

fBody :: TestTerm
fBody = "\\(a :: Int) -> let { (r :: Int) = a } in r"

-- | The declarations of the first function argument of a blackbox context
functionDecls :: BlackBoxContext -> [Declaration]
functionDecls bbCtx = case IntMap.lookup 0 (bbFunctions bbCtx) of
  Just ((Right (_, decls), _, _, _, _, _) : _) -> decls
  _ -> error "Expected the declarations of a function argument"

-- | A local variable whose raw name is the unique of a global binder is not
-- an instance of that binder: a local function is a 'Var', which can't be
-- turned into a component, so this is an error.
mkFunInputLocalShadowingGlobal :: Assertion
mkFunInputLocalShadowingGlobal = do
  -- The local f has the raw name of the unique of the global f
  result <- try $ withTestScope [atRaw 60 (varD "f" "Int -> Int"), varD "res" "Int"] $ \ts ->
    case termIn ts "res" of
      Var o ty n ->
        runScopedNetlistTest (mkGlobalBindings [(fGlobal, fBody)]) $ do
          componentNames .= singletonGlobalEnv fGlobal (Id.unsafeMake "f")
          mkBlackBoxContext (testCompCtx ts) "prim" Concurrent [LocalId o ty n] [Left (termIn ts "f")]
      _ -> error "unexpected terms"
  case result of
    Left (ErrorCall msg)
      | "Cannot make function input" `List.isInfixOf` msg -> pure ()
      | otherwise -> assertFailure ("Unexpected error:\n" <> msg)
    Right _ -> assertFailure "Expected an error, but got a blackbox context"

-- | A global function argument becomes an instance of its component
mkFunInputGlobal :: Assertion
mkFunInputGlobal = do
  bbCtx <- withTestScope [varD "res" "Int"] $ \ts -> case termIn ts "res" of
    Var o ty n ->
      fmap fst $ runScopedNetlistTest (mkGlobalBindings [(fGlobal, fBody)]) $ do
        componentNames .= singletonGlobalEnv fGlobal (Id.unsafeMake "f")
        mkBlackBoxContext (testCompCtx ts) "prim" Concurrent [LocalId o ty n] [Left (GlobalVar fGlobal)]
    _ -> error "unexpected terms"
  case functionDecls bbCtx of
    [InstDecl _ _ _ compName _ _ _] ->
      assertEqual "Instantiated component" "f" (Id.toText compName)
    decls -> assertFailure ("Expected an instance of f, but got: " <> show decls)

-- | The variables of the lambdas of a function argument get the HDL
-- identifiers of the arguments of the function in the template, and those of
-- its let-bindings unique ones
mkFunInputLambda :: Assertion
mkFunInputLambda = do
  bbCtx <- withTerms ["\\(a :: Int) -> let { (r :: Int) = a } in r", "res :: Int"] $ \ts tms ->
    case tms of
      [f, Var o ty n] ->
        fst <$> runScopedNetlistTest emptyGlobalEnv
          (mkBlackBoxContext (testCompCtx ts) "prim" Concurrent [LocalId o ty n] [Left f])
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
