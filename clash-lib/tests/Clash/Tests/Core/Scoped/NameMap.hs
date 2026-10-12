{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for the sets and maps of names of scoped Clash Core
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Clash.Tests.Core.Scoped.NameMap (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C

import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.NameMap
import Clash.Core.Scoped.Subst
import Clash.Core.Scoped.Syntax

import Test.Clash.Rewrite (assertErrorContains, globalId, intId, intTy)

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.NameMap"
  [ testCase "GlobalEnv" $ do
      let f = mkGlobalIdFromClash (globalId C.User "f" 3 intTy)
          g = mkGlobalIdFromClash (globalId C.User "g" 2 intTy)
          env = insertGlobalEnv f 'f' (singletonGlobalEnv g 'g')
      lookupGlobalEnv f env @?= Just 'f'
      lookupGlobalEnv g (deleteGlobalEnv g env) @?= Nothing
      -- In the order of the uniques
      globalEnvToList env @?= [(g, 'g'), (f, 'f')]
      globalEnvToList (insertWithGlobalEnv (++) f "x" (fmap pure env)) @?= [(g, "g"), (f, "xf")]
      globalSetToList (globalEnvKeysSet env) @?= [g, f]
      globalEnvKeys (withoutGlobalEnv env (singletonGlobalSet g)) @?= [f]

  , testCase "mkGlobalIdFromClash errors on local variables" $
      assertErrorContains "not a global variable"
        (C.Var (globalIdLegacy (mkGlobalIdFromClash (intId C.User "x" 1))))

  , testCase "NameSet and NameMap" $
      case fromClashTermClosed (C.Lam (intId C.User "x" 1) (C.Var (intId C.User "x" 1))) of
        Lam b e -> openIdBinder emptyScope b e $ \_ b1 _ -> do
          let n = tmNameOf b1
              m = singletonNameMap n 'x'
          lookupNameMap n m @?= Just 'x'
          nameMapKeys m @?= [n]
          -- Out of the scope of the binder, the map is empty
          sizeNameMap (unsinkNameMap (idBinderNameBinders b1) m) @?= 0
          sizeNameSet (unsinkNameSet (idBinderNameBinders b1) (singletonNameSet n)) @?= 0
        _ -> assertFailure "unexpected shape"

  , testCase "appendNameBinders" $ do
      let x = intId C.User "x" 1
          y = intId C.User "y" 2
      case fromClashTermClosed (C.Lam x (C.Lam y (C.Var x))) of
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
