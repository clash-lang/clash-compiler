{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.SignalPortal where

import Test.Tasty
import Test.Tasty.Hedgehog (testPropertyNamed)

import Clash.Signal (Signal, System)
import Clash.Signal.Internal (fromList, sampleN)
import Clash.Signal.Portal
import Clash.Sized.Unsigned (Unsigned)

import qualified Hedgehog as H

type SignalData = Unsigned 8

sourceSignal :: Signal System SignalData
sourceSignal = fromList [10, 11 ..]

portalRoundTrip :: H.Property
portalRoundTrip =
  H.property $ do
    let
      sourced = portalSource "portalRoundTrip" sourceSignal
      sink = portalSink "portalRoundTrip" :: Signal System SignalData

    sampleN 4 sourced H.=== [10, 11, 12, 13]
    sampleN 4 sink H.=== [10, 11, 12, 13]

repeatedPortalSource :: H.Property
repeatedPortalSource =
  H.property $ do
    let
      sourced0 = portalSource "repeatedPortalSource" sourceSignal
      sourced1 = portalSource "repeatedPortalSource" sourceSignal
      sink = portalSink "repeatedPortalSource" :: Signal System SignalData

    sampleN 4 sourced0 H.=== [10, 11, 12, 13]
    sampleN 4 sourced1 H.=== [10, 11, 12, 13]
    sampleN 4 sink H.=== [10, 11, 12, 13]

tests :: TestTree
tests =
  testGroup
    "SignalPortal"
    [ testPropertyNamed "portalRoundTrip" "portalRoundTrip" portalRoundTrip
    , testPropertyNamed "repeatedPortalSource" "repeatedPortalSource" repeatedPortalSource
    ]
