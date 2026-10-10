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

import Clash.Core.Scoped.NameMap
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

  ]
