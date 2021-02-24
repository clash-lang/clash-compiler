{-# LANGUAGE DeriveAnyClass #-}

module Clash.Tests.AutoReg where

import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Data.List as L
import Clash.Prelude

test :: (Eq a, Show a, AutoReg a, Arbitrary a) => a -> [a] -> Property
test initVal xs = testFor (L.length xs) $ register initVal input .==. autoReg initVal input
  where input = fromList @_ @System xs

tests :: TestTree
tests =
  testGroup
    "AutoReg"
      [ testGroup "autoReg === register"
        [ testProperty "Int" $ test @Int
        , testProperty "(Unsigned 4, Bool)" $ test @(Unsigned 4, Bool)
        , testProperty "Maybe Bool" $ test @(Maybe Bool)
        -- , testProperty "Either Bool (Unsigned 3)" $ test @(Either Bool (Unsigned 3))

        , testProperty "Vec 4 Bool" $ test @(Vec 4 Bool)
        , testProperty "Vec 4 (Maybe Bool)" $ test @(Vec 4 (Maybe Bool))

        , testProperty "Maybe (Vec 4 Bool)" $ test @(Maybe (Vec 4 Bool))
        , testProperty "Maybe (Maybe (Vec 4 Bool))" $ test @(Maybe (Maybe (Vec 4 Bool)))

        , testProperty "RTree 2 Bool" $ test @(RTree 2 Bool)
        , testProperty "Maybe (RTree 2 Bool)" $ test @(Maybe (RTree 2 Bool))
        , testProperty "Maybe (Maybe (RTree 2 Bool))" $ test @(Maybe (Maybe (RTree 2 Bool)))

        , testProperty "Maybe (Vec 4 (Maybe (Vec 3 Bool)))" $ test @(Maybe (Vec 4 (Maybe (Vec 3 Bool))))
        ]
      ]
