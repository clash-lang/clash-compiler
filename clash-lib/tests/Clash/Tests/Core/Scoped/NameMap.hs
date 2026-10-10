{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for the sets and maps of names of scoped Clash Core
-}

{-# LANGUAGE GADTs #-}

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
      let f = mkGlobalId (globalId C.User "f" 3 intTy)
          g = mkGlobalId (globalId C.User "g" 2 intTy)
          env = insertGlobalEnv f 'f' (singletonGlobalEnv g 'g')
      lookupGlobalEnv f env @?= Just 'f'
      lookupGlobalEnv g (deleteGlobalEnv g env) @?= Nothing
      -- In the order of the uniques
      globalEnvToList env @?= [(g, 'g'), (f, 'f')]
      globalEnvToList (insertWithGlobalEnv (++) f "x" (fmap pure env)) @?= [(g, "g"), (f, "xf")]
      globalSetToList (globalEnvKeysSet env) @?= [g, f]
      globalEnvKeys (withoutGlobalEnv env (singletonGlobalSet g)) @?= [f]

  , testCase "mkGlobalId errors on local variables" $
      assertErrorContains "not a global variable"
        (C.Var (globalIdVar (mkGlobalId (intId C.User "x" 1))))

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
  ]
