module Test.Cores.Xilinx.DnaPortE2 where
import Clash.Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe

import Clash.Cores.Xilinx.Unisim.DnaPortE2

import qualified Data.List as L

-- Simulate readDnaPortE2 and check if it produces the simulation DNA within
-- 100 cycles
testDeviceDna :: TestTree
testDeviceDna = testCase "readDnaPortE2 produces simulation DNA" $ do
  let
    result = catMaybes $ sampleN @System 100 $
      readDnaPortE2 hasClock hasReset hasEnable simDna2
  assertBool "No results produced" (not $ null result)
  assertEqual "Unexpected result" result (L.replicate (L.length result) simDna2)

-- Add this test to your existing test group
tests :: TestTree
tests = testGroup "Xilinx DNA tests" [testDeviceDna]
