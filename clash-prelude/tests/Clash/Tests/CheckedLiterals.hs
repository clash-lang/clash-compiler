{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

module Clash.Tests.CheckedLiterals (tests) where

import Test.Tasty (TestTree, testGroup)

import qualified Clash.Tests.CheckedLiterals.BitVector
import qualified Clash.Tests.CheckedLiterals.Fixed
import qualified Clash.Tests.CheckedLiterals.Index
import qualified Clash.Tests.CheckedLiterals.Signals
import qualified Clash.Tests.CheckedLiterals.Signed
import qualified Clash.Tests.CheckedLiterals.Unsigned
import qualified Clash.Tests.CheckedLiterals.Wrappers

tests :: TestTree
tests = testGroup "CheckedLiterals"
  [ Clash.Tests.CheckedLiterals.BitVector.tests
  , Clash.Tests.CheckedLiterals.Unsigned.tests
  , Clash.Tests.CheckedLiterals.Signed.tests
  , Clash.Tests.CheckedLiterals.Index.tests
  , Clash.Tests.CheckedLiterals.Fixed.tests
  , Clash.Tests.CheckedLiterals.Wrappers.tests
  , Clash.Tests.CheckedLiterals.Signals.tests
  ]
