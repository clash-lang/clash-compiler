{-|
Copyright  :  (C) 2022, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Convenience functions \"missing\" in "Test.Tasty.Hedgehog"
-}

module Test.Tasty.Hedgehog.Extra (testPropertyXXX) where

import Data.String (IsString(fromString))
import Hedgehog (Property)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testPropertyNamed)

-- | 'Test.Tasty.Hedgehog.testProperty' has been deprecated in favor of
-- 'testPropertyNamed', but we've written our test cases in such a way that using
-- it correctly is hard. To prevent deprecation warnings, we apply this workaround.
testPropertyXXX :: TestName -> Property -> TestTree
testPropertyXXX nm prop = testPropertyNamed nm (fromString nm) prop
