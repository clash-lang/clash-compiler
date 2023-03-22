module Main where

import Test.Tasty

import qualified Clash.Tests.AsyncFIFOSynchronizer
import qualified Clash.Tests.AutoReg
import qualified Clash.Tests.BitPack
import qualified Clash.Tests.BitVector
import qualified Clash.Tests.BlockRam
import qualified Clash.Tests.BlockRam.Blob
import qualified Clash.Tests.Clocks
import qualified Clash.Tests.Counter
import qualified Clash.Tests.DerivingDataRepr
import qualified Clash.Tests.Fixed
import qualified Clash.Tests.FixedExhaustive
import qualified Clash.Tests.MaybeX
import qualified Clash.Tests.NFDataX
import qualified Clash.Tests.NumNewtypes
import qualified Clash.Tests.Ram
import qualified Clash.Tests.Reset
import qualified Clash.Tests.Resize
import qualified Clash.Tests.Signal
import qualified Clash.Tests.Signed
import qualified Clash.Tests.TopEntityGeneration
import qualified Clash.Tests.Unsigned
import qualified Clash.Tests.Vector

import qualified Clash.Tests.Laws.Enum
import qualified Clash.Tests.Laws.SaturatingNum

tests :: TestTree
tests = testGroup "Unittests"
  [ Clash.Tests.AsyncFIFOSynchronizer.tests
  , Clash.Tests.AutoReg.tests
  , Clash.Tests.BitPack.tests
  , Clash.Tests.BitVector.tests
  , Clash.Tests.BlockRam.tests
  , Clash.Tests.BlockRam.Blob.tests
  , Clash.Tests.Clocks.tests
  , Clash.Tests.Counter.tests
  , Clash.Tests.DerivingDataRepr.tests
  , Clash.Tests.Fixed.tests
  , Clash.Tests.FixedExhaustive.tests
  , Clash.Tests.MaybeX.tests
  , Clash.Tests.NFDataX.tests
  , Clash.Tests.NumNewtypes.tests
  , Clash.Tests.Ram.tests
  , Clash.Tests.Reset.tests
  , Clash.Tests.Resize.tests
  , Clash.Tests.Signal.tests
  , Clash.Tests.Signed.tests
  , Clash.Tests.TopEntityGeneration.tests
  , Clash.Tests.Unsigned.tests
  , Clash.Tests.Vector.tests
  , testGroup "Laws"
    [ Clash.Tests.Laws.Enum.tests
    , Clash.Tests.Laws.SaturatingNum.tests
    ]
  ]

main :: IO ()
main = defaultMain tests
