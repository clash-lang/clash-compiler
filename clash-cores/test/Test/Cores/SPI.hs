module Test.Cores.SPI where

import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

import           Clash.Prelude hiding (interleave)

import           Test.Cores.Internal.SampleSPI
import           Test.Cores.Internal.Signals

data Device
  = Master | Slave
  deriving (Eq, Show)

instance Arbitrary Device where
  arbitrary = QC.elements [Master, Slave]

-- | Test that sending a simple input repeats that simple input
-- without changing it. The lawfulness of a device is irrelevant here.
--
simpleRepeats
  :: (1 <= halfPeriod, 1 <= waitTime)
  => SimpleTest halfPeriod waitTime 8 8 (Device -> Bool)
simpleRepeats divHalf wait mVal sVal mode latch duration dev =
  case dev of
    Master -> checkWith ssMasterOut sVal
    Slave  -> checkWith ssSlaveOut  mVal
 where
  sampleSPI = sampleSimple masterLawfulSignal slaveLawfulSignal

  checkWith outs val =
    let spis = sampleSPI divHalf wait mVal sVal mode latch duration
     in all (== val) . catMaybes $ outs spis

-- | Test that sending muliple words repeats those words in a cycle
-- without changing the message. This only works for lawful devices.
--
cyclingRepeats
  :: (1 <= halfPeriod, 1 <= waitTime)
  => CyclingTest halfPeriod waitTime 8 8 (Device -> Bool)
cyclingRepeats divHalf wait mVals sVals mode latch duration dev =
  case dev of
    Master -> checkWith ssMasterOut sVals
    Slave  -> checkWith ssSlaveOut  mVals
 where
  sampleSPI = sampleCycling masterLawfulSignal slaveLawfulSignal

  checkWith outs vals =
    let spis = sampleSPI divHalf wait mVals sVals mode latch duration
     in NE.isPrefixOf (catMaybes $ outs spis) (NE.cycle vals)

-- | Test that acknowledgement signals and done signals are interleaved.
-- As acknowledgement signals a transfer has started and done signals a
-- transfer has finished, there should never be two of either in a row.
-- The lawfulness of a device is irrelevant here.
--
ackDoneInterleaved
  :: (1 <= halfPeriod, 1 <= waitTime)
  => SimpleTest halfPeriod waitTime 8 8 (Device -> Bool)
ackDoneInterleaved divHalf wait mVal sVal mode latch duration dev =
  case dev of
    Master -> checkWith ssMasterOut ssMasterAck
    Slave  -> checkWith ssSlaveOut  ssSlaveAck
 where
  sampleSPI = sampleSimple masterLawfulSignal slaveLawfulSignal

  interleave []     ys = ys
  interleave (x:xs) ys = x : interleave ys xs

  checkWith outs acks =
    let spis   = sampleSPI divHalf wait mVal sVal mode latch duration
        outIxs = List.findIndices isJust $ outs spis
        ackIxs = List.elemIndices True $ acks spis
     in interleave ackIxs outIxs == List.sort (ackIxs <> outIxs)

tests :: TestTree
tests = testGroup "SPI"
  [ testGroup "Repeating messages"
    [ QC.testProperty "Single word messages repeat indefinitely"
        $ simpleRepeats d4 d1 mSimple sSimple

    , QC.testProperty "Multiple word messages repeat indefinitely"
        $ cyclingRepeats d4 d1 mCycling sCycling
    ]

  , testGroup "Acknowledgement signals"
    [ QC.testProperty "Acknowledgement interleaves with Done"
        $ ackDoneInterleaved d4 d1 mSimple sSimple
    ]
  ]
 where
  mSimple  = 0b10101010
  mCycling = 0b10101010 :| [0b11110000, 0b10010110]

  sSimple  = 0b00010011
  sCycling = 0b11111111 :| [0b00100010]
