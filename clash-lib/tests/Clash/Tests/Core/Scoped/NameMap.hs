{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for the sets and maps of names of scoped Clash Core
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Scoped.NameMap (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Scoped.NameMap
import Clash.Core.Scoped.Subst
import Clash.Core.Scoped.Syntax

import Test.Clash.Scoped (closedTerm, testGlobalId)

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.NameMap"
  [ testCase "GlobalEnv" $ do
      let f = testGlobalId "f_G3" "Int"
          g = testGlobalId "g_G2" "Int"
          env = insertGlobalEnv f 'f' (singletonGlobalEnv g 'g')
      lookupGlobalEnv f env @?= Just 'f'
      lookupGlobalEnv g (deleteGlobalEnv g env) @?= Nothing
      -- In the order of the uniques
      globalEnvToList env @?= [(g, 'g'), (f, 'f')]
      globalEnvToList (insertWithGlobalEnv (++) f "x" (fmap pure env)) @?= [(g, "g"), (f, "xf")]
      globalSetToList (globalEnvKeysSet env) @?= [g, f]
      globalEnvKeys (withoutGlobalEnv env (singletonGlobalSet g)) @?= [f]


  , testCase "NameSet and NameMap" $
      case closedTerm "\\(x :: Int) -> x" of
        Lam b e -> openIdBinder emptyScope b e $ \_ b1 _ -> do
          let n = tmNameOf b1
              m = singletonNameMap n 'x'
          lookupNameMap n m @?= Just 'x'
          nameMapKeys m @?= [n]
          -- Out of the scope of the binder, the map is empty
          sizeNameMap (unsinkNameMap (idBinderNameBinders b1) m) @?= 0
          sizeNameSet (unsinkNameSet (idBinderNameBinders b1) (singletonNameSet n)) @?= 0
        _ -> assertFailure "unexpected shape"

  , testCase "appendNameBinders" $
      case closedTerm "\\(x :: Int) (y :: Int) -> x" of
        Lam bx (Lam by e) -> do
          let nbs = appendNameBinders (idBinderNameBinders bx) (idBinderNameBinders by)
          -- Both variables are bound between the scopes
          sizeNameSet (unsinkNameSet nbs (freeTmVarsSet e)) @?= 0
          -- Only x occurs in the body
          assertBool "x occurs" (null (unsinkTerm nbs e))
          assertBool "y doesn't occur"
            (not (null (unsinkTerm (idBinderNameBinders by) e)))
        _ -> assertFailure "unexpected shape"
  ]
 where
  freeTmVarsSet :: Term n -> NameSet TmName n
  freeTmVarsSet = \case
    Var _ _ n -> singletonNameSet n
    _ -> emptyNameSet
