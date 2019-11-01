module Main where

import Test.Tasty

import qualified Clash.Tests.AutoReg
import qualified Clash.Tests.BitPack
import qualified Clash.Tests.BitVector
import qualified Clash.Tests.DerivingDataRepr
import qualified Clash.Tests.Signal
import qualified Clash.Tests.NFDataX
import qualified Clash.Tests.TopEntityGeneration

tests :: TestTree
tests = testGroup "Unittests"
  [ Clash.Tests.AutoReg.tests
  , Clash.Tests.BitPack.tests
  , Clash.Tests.BitVector.tests
  , Clash.Tests.DerivingDataRepr.tests
  , Clash.Tests.Signal.tests
  , Clash.Tests.NFDataX.tests
  , Clash.Tests.TopEntityGeneration.tests
  ]

main :: IO ()
main = defaultMain tests
