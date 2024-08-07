module Test.Cores.Xilinx.Ethernet.Gmii where

import Clash.Prelude

import Clash.Cores.Xilinx.Ethernet.Gmii.Internal
import Test.Tasty
import Test.Tasty.HUnit

-- | Verify that the packing of @LinkSpeed@ matches the layout corresponding to the
-- @an_adv_config_vector@.
testLinkSpeedPacking :: TestTree
testLinkSpeedPacking = testCase "LinkSpeed packing" $ do
  let
    speeds = [Speed10, Speed100, Speed1000]
    packed = fmap pack speeds
  assertEqual "Unexpected packing" [0, 1, 2] packed

-- | Verify that the packing of @Pause@ matches the layout corresponding to the
-- @an_adv_config_vector@.
testPausePacking :: TestTree
testPausePacking = testCase "Pause packing" $ do
  let
    pauses = [NoPause, SymmetricPause, AsymmetricPause, SymmetricAsymmetricPause]
    packed = fmap pack pauses
  assertEqual "Unexpected packing" [0, 1, 2, 3] packed

-- | Verify that the packed representation of @DuplexMode@ matches the layout
-- corresponding to the @an_adv_config_vector@.
testDuplexModePacking :: TestTree
testDuplexModePacking = testCase "DuplexMode packing" $ do
  let
    modes = [HalfDuplex, FullDuplex]
    packed = fmap pack modes
  assertEqual "Unexpected packing" [0, 1] packed

-- | Verify that the packed representation of @Config@ matches the layout
-- corresponding to the @configuration_vector@.
testConfigPacking :: TestTree
testConfigPacking = testCase "Config packing" $ do
  assertEqual "Default" 0 (pack (def :: Config))
  assertSetBit "Unidirectional" 0 (pack $ def { cUnidirectional = True })
  assertSetBit "Loopback" 1 (pack $ def { cLoopback = True })
  assertSetBit "PowerDown" 2 (pack $ def { cPowerDown = True })
  assertSetBit "IsolateGmii" 3 (pack $ def { cIsolateGmii = True })
  assertSetBit "AutoNegEnable" 4 (pack $ def { cAutoNegEnable = True })
 where
  -- Assert that setting bit i in the default configuration results in the
  -- expected configuration.
  assertSetBit str (i :: Int) = assertEqual str (replaceBit i 1 def)

-- | Verify that the packed representation of @AutoNegConfig@ matches the layout
-- corresponding to the @an_adv_config_vector@.
testAutoNegConfigPacking :: TestTree
testAutoNegConfigPacking = testCase "AutoNegConfig packing" $ do
  assertEqual "Default" (AutoNegConfigVector 1) (toAutoNegConfigVector def)
  assertSetBit "LinkSpeed100" 10 (def{ cLinkSpeed = Speed100 })
  assertSetBit "LinkSpeed1000" 11 (def{ cLinkSpeed = Speed1000 })
  assertSetBit "DuplexMode" 12 (def{ cDuplexMode = FullDuplex })
  assertSetBit "Acknowledge" 14 (def { cAcknowledge = True })
  assertSetBit "PhyLinkStatus" 15 (def { cPhyLinkStatus = True })
 where
  -- Assert that setting bit i in the default configuration results in the
  -- expected configuration.
  assertSetBit str (i :: Int) expected =
    assertEqual str (replaceBit i 1 (toAutoNegConfigVector def))
    (toAutoNegConfigVector expected)

-- Collect all tests in a single test group
tests :: TestTree
tests = testGroup "Ethernet GMII tests"
  [ testLinkSpeedPacking
  , testPausePacking
  , testDuplexModePacking
  , testConfigPacking
  , testAutoNegConfigPacking
  ]
