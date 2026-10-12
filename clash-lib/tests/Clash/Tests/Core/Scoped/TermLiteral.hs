{-|
Copyright   :  (C) 2022, Google Inc.
                   2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Tests for 'Clash.Core.Scoped.TermLiteral', the scoped twins of
"Clash.Tests.Core.TermLiteral".
-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Tests.Core.Scoped.TermLiteral where

import Data.Proxy
import Data.Typeable
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import qualified Clash.Core.Literal as L
import Clash.Core.Scoped.Syntax (S (VoidS), Term (App, Literal, Tick), TickInfo (NoDeDup))
import Clash.Core.Scoped.TermLiteral
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

-- Regression test for #2926: 'termToData' must be transparent to ticks
-- anywhere in the term.
case_termToData_strips_ticks_in_subterm :: Assertion
case_termToData_strips_ticks_in_subterm =
  Just "hi"
    @=? either (const Nothing) Just (termToData @String term)
 where
  term :: Term 'VoidS
  term =
    App (Literal (L.IntegerLiteral 0))
        (Tick NoDeDup (Literal (L.StringLiteral "hi")))

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.TermLiteral" [$(testGroupGenerator)]
