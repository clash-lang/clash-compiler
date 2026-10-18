{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Util (tests) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Clash.Core.Literal      (Literal(..))
import           Clash.Core.Name         (Name(..), NameSort(..), OccName)
import           Clash.Core.Term         (Bind(..), Term(..))
import           Clash.Core.Util         (renameIds)
import           Clash.Core.Var          (Id, Var(..))
import           Clash.Core.VarEnv       (unitVarEnv)

import           Test.Clash.Rewrite      (intTy, localId)

x, y :: Id
x = localId System "x" 1 intTy
-- | 'x', renamed
y = x {varName = (varName x) {nameOcc = "y"}}

nameOf :: Id -> OccName
nameOf = nameOcc . varName

-- | In @let x = 5 in (\x -> x) x@, where both binders of @x@ have the same
-- unique, only the outer @x@ and the occurrences referring to it are renamed.
renameIdsShadowing :: Assertion
renameIdsShadowing =
  case renameIds (unitVarEnv x y) tm of
    Let (NonRec b0 _) (App (Lam b1 (Var o1)) (Var o0)) ->
      map nameOf [b0, o0, b1, o1] @?= ["y", "y", "x", "x"]
    tm1 -> assertFailure ("Unexpected shape: " <> show tm1)
 where
  tm = Let (NonRec x (Literal (IntLiteral 5))) (App (Lam x (Var x)) (Var x))

-- | Occurrences of variables that aren't bound in the term keep their names
renameIdsFree :: Assertion
renameIdsFree =
  case renameIds (unitVarEnv x y) (Var x) of
    Var o -> nameOf o @?= "x"
    tm1 -> assertFailure ("Unexpected shape: " <> show tm1)

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Core.Util"
    [ testGroup "renameIds"
      [ testCase "Shadowing" renameIdsShadowing
      , testCase "Free variables" renameIdsFree
      ]
    ]
