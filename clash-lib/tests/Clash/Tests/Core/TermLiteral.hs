{-|
Copyright   :  (C) 2022, Google Inc.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Tests for 'Clash.Core.TermLiteral'.
-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Tests.Core.TermLiteral where

import Data.Proxy
import Data.Typeable
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Clash.Core.TermLiteral
import Clash.Core.TermLiteral.TH
import Clash.Promoted.Nat

import Clash.Tests.Core.TermLiteral.Types

showTypeable :: Typeable a => Proxy a -> String
showTypeable proxy = showsPrec 0 (typeRep proxy) ""

eqTest :: (TermLiteral a, Typeable a) => Proxy a -> Assertion
eqTest proxy = showType proxy @=? showTypeable proxy

case_int :: Assertion
case_int = eqTest (Proxy @Int)

case_maybe_int :: Assertion
case_maybe_int = eqTest (Proxy @(Maybe Int))

case_maybe_maybe_int :: Assertion
case_maybe_maybe_int = eqTest (Proxy @(Maybe (Maybe Int)))

case_either_int_int :: Assertion
case_either_int_int = eqTest (Proxy @(Either Int Int))

case_either_int_maybe_int :: Assertion
case_either_int_maybe_int = eqTest (Proxy @(Either Int (Maybe Int)))

case_int_int :: Assertion
case_int_int = eqTest (Proxy @(Int, Int))

case_maybe_int_maybe_int :: Assertion
case_maybe_int_maybe_int = eqTest (Proxy @(Maybe Int, Maybe Int))

case_maybe_int_int :: Assertion
case_maybe_int_int = eqTest (Proxy @(Maybe (Int, Int)))

case_snat :: Assertion
case_snat = "SNat _" @=? showType (Proxy @(SNat 5))

case_maybe_snat :: Assertion
case_maybe_snat = "Maybe (SNat _)" @=? showType (Proxy @(Maybe (SNat 5)))

deriveTermLiteral ''NatTypeArg

case_natTypeArg :: Assertion
case_natTypeArg = "NatTypeArg _" @=? showType (Proxy @(NatTypeArg 10))

tests :: TestTree
tests = testGroup "Clash.Tests.Core.TermLiteral" [$(testGroupGenerator)]
