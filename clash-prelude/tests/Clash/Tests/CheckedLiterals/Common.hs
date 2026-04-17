{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE QuasiQuotes #-}

module Clash.Tests.CheckedLiterals.Common where

import Data.List (intercalate)
import Data.String.Interpolate (__i)
import Test.Tasty (TestTree)
import Test.Tasty.AssertGhc (Expected (..), testCaseGhc)

-- | First tuple element may list several modules separated by @", "@; each one
-- gets its own @import@ line in the generated snippet. This lets a test case
-- reference types that live in different modules (e.g. a @Clash.Num.*@ wrapper
-- around a @Clash.Sized.*@ type).
toTestCases :: [(String, String, String, [String])] -> [TestTree]
toTestCases = map toTestCase

toTestCase :: (String, String, String, [String]) -> TestTree
toTestCase (moduleNames, typeName, literal, expectedErrors) =
  testCaseGhc
    ((if null expectedErrors then "OK , " else "NOK, ") ++ typeName ++ ", " ++ literal)
    [__i|
      import Prelude
      #{imports}
      import GHC.TypeNats
      test :: #{typeName}
      test = #{literal}
    |]
    ( if null expectedErrors
        then ExpectSuccess
        else ExpectFailure expectedErrors
    )
 where
  imports = intercalate "\n" ["import " ++ m | m <- splitOnCommaSpace moduleNames]

  splitOnCommaSpace :: String -> [String]
  splitOnCommaSpace s = case break (== ',') s of
    (x, "")       -> [x]
    (x, ',':' ':r) -> x : splitOnCommaSpace r
    (x, ',':r)    -> x : splitOnCommaSpace r
    (x, _)        -> [x]
