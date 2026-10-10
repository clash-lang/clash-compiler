{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for scopes represented by a watermark, see
  "Clash.Core.Scoped.Internal.Scope"
-}

module Clash.Tests.Core.Scoped.Scope (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Scoped.Internal.Scope

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Scope"
  [ testCase "withFresh takes the name just above the watermark" $
      withFresh emptyScope $ \b1 -> do
        nameId (nameOf b1) @?= 1
        let sc1 = extendScope b1 emptyScope
        scopeWatermark sc1 @?= 1
        withFresh sc1 $ \b2 -> nameId (nameOf b2) @?= 2

  , testCase "withRefreshed keeps a name above the watermark" $
      withRefreshed emptyScope (UnsafeName 10 :: Name 'VoidS) $ \b1 -> do
        nameId (nameOf b1) @?= 10
        let sc1 = extendScope b1 emptyScope
        -- A name at or below the watermark is renamed
        withRefreshed sc1 (UnsafeName 10 :: Name 'VoidS) $ \b2 -> nameId (nameOf b2) @?= 11
        withRefreshed sc1 (UnsafeName 3 :: Name 'VoidS) $ \b2 -> nameId (nameOf b2) @?= 11

  , testCase "withFreshRaws checks every name against the names before it" $ do
      let raws :: [Int] -> (Int -> (Int -> a) -> Int -> a) -> (Int -> a) -> Int -> a
          raws ks step done = foldr step done ks
          fresh ks = withFreshRaws emptyScope (raws ks) scopeWatermark (-1)
      fresh [1, 2, 5] @?= 5
      fresh [] @?= 0
      -- Not increasing: the second name may clash with the first
      fresh [2, 2] @?= -1
      fresh [3, 1] @?= -1
      fresh [0] @?= -1
  ]
