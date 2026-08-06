{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE MagicHash #-}

module Clash.Tests.Util (tests) where

import GHC.Exts (Word(W#))
import qualified GHC.Num.Integer as Integer (integerLogBase#)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Clash.Util (clogBase, flogBase, logBaseOutOfDomain)

-- | Ground truth: what GHC computes for @integerLogBase# base y@ at runtime.
-- Note that this is the very same function 'flogBase' calls, so this pins
-- 'logBaseOutOfDomain' to GHC's behaviour, whatever that behaviour is.
ghcLogBase :: Integer -> Integer -> Integer
ghcLogBase base y = toInteger (W# (Integer.integerLogBase# base y))

tests :: TestTree
tests = testGroup "Clash.Util"
  [ testGroup "logBaseOutOfDomain"
    -- The evaluator only applies 'logBaseOutOfDomain' when 'flogBase' returns
    -- 'Nothing' while the base is greater than one, i.e. when @y <= 0@. See
    -- 'Clash.GHC.Evaluator.Primitive'.
    [ testProperty "matches GHC for y <= 0" $
        \(getPositive -> base') (getNonNegative -> y') ->
          let base = base' + 1  -- base > 1
              y = negate y'     -- y <= 0
           in logBaseOutOfDomain base y === ghcLogBase base y

    , testCase "base 2, y == 0 underflows to maxBound" $
        logBaseOutOfDomain 2 0 @?= toInteger (maxBound :: Word)

    , testCase "base 3, y == 0 is zero" $
        logBaseOutOfDomain 3 0 @?= 0

    , testCase "negative y is zero" $
        map (`logBaseOutOfDomain` (-1)) [2, 3, 10] @?= [0, 0, 0]
    ]

  , testGroup "flogBase"
    [ testProperty "matches GHC inside its domain" $
        \(getPositive -> base') (getPositive -> y) ->
          let base = base' + 1
           in fmap toInteger (flogBase base y) === Just (ghcLogBase base y)

    , testProperty "is Nothing outside its domain" $
        \base y ->
          not (base > 1 && y > 0) ==> flogBase base y === Nothing
    ]

  , testGroup "clogBase"
    [ testProperty "is the ceiling of the base-x logarithm" $
        \(getPositive -> base') (getPositive -> y) ->
          let base = base' + 1
           in case clogBase base y of
                Nothing -> property False
                Just c ->
                  -- base^c is the smallest power of base that is >= y
                  property (base ^ c >= y && (c == 0 || base ^ (c - 1) < y))
    ]
  ]
