{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Driver.Manifest where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import Data.Coerce (coerce)
import Data.Either (fromRight)
import Data.Text (Text)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Real (Ratio((:%)))

import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Test.Tasty.QuickCheck as Q
import qualified Test.QuickCheck.Utf8 as Q

import Clash.Driver.Manifest
import Clash.Explicit.Signal

newtype ArbitraryText = ArbitraryText Text deriving (Show)

instance Q.Arbitrary ArbitraryText where
  arbitrary = coerce Q.genValidUtf8
  shrink = coerce Q.shrinkValidUtf8

newtype ArbitraryPortDirection = ArbitraryPortDirection PortDirection deriving (Show)

instance Q.Arbitrary ArbitraryPortDirection where
  arbitrary = ArbitraryPortDirection <$> Q.elements [In, Out, InOut]

genDigest :: Q.Gen ByteString
genDigest = Base16.encode . Text.encodeUtf8 . coerce @ArbitraryText <$> Q.arbitrary

genString :: Q.Gen FilePath
genString = Text.unpack . coerce @ArbitraryText <$> Q.arbitrary

genDomain :: Q.Gen (Text, VDomainConfiguration)
genDomain = do
  nm <- coerce @(Q.Gen ArbitraryText) Q.arbitrary
  dom <-
    VDomainConfiguration
      <$> pure (Text.unpack nm)
      <*> (fromIntegral @Int . abs <$> Q.arbitraryBoundedIntegral)
      <*> Q.elements [Rising, Falling]
      <*> Q.elements [Synchronous, Asynchronous]
      <*> Q.elements [Defined, Unknown]
      <*> Q.elements [ActiveHigh, ActiveLow]

  pure (nm, dom)

genPort :: Q.Gen ManifestPort
genPort =
  ManifestPort
    <$> coerce @(Q.Gen ArbitraryText) Q.arbitrary
    <*> coerce @(Q.Gen ArbitraryText) Q.arbitrary
    <*> coerce @(Q.Gen ArbitraryPortDirection) Q.arbitrary
    <*> (fromIntegral @Int . abs <$> Q.arbitraryBoundedIntegral)
    <*> Q.elements [False, True]
    <*> coerce @(Q.Gen (Maybe ArbitraryText)) Q.arbitrary

genDebugSubHashes :: Q.Gen DebugSubHashes
genDebugSubHashes =
  DebugSubHashes
    <$> genDigest
    <*> genDigest
    <*> genDigest
    <*> genDigest
    <*> genDigest

genManifest :: Q.Gen Manifest
genManifest =
  Manifest
    <$> genDigest -- hash
    <*> Q.oneof [pure Nothing, Just <$> genDebugSubHashes] -- __debug_hash
    <*> Q.arbitrary -- flags
    <*> Q.listOf genPort -- ports
    <*> coerce @(Q.Gen [ArbitraryText]) @(Q.Gen [Text]) Q.arbitrary -- comp names
    <*> coerce @(Q.Gen ArbitraryText)   @(Q.Gen Text)   Q.arbitrary -- top name
    <*> Q.listOf ((,) <$> genString <*> genDigest) -- files
    <*> (HashMap.fromList <$> Q.listOf genDomain) -- domains
    <*> coerce @(Q.Gen [ArbitraryText]) @(Q.Gen [Text]) Q.arbitrary -- dependencies

tests :: TestTree
tests =
  adjustOption (\_ -> Q.QuickCheckTests 100) $
  testGroup
    "Clash.Tests.Driver.Manifest"
    [ Q.testProperty "decode . encode ~ id" $ do
        manifest <- genManifest
        let
          encoded = Aeson.encodePretty manifest
          decoded = Aeson.eitherDecode encoded

        pure (decoded Q.=== Right manifest)
    , Q.testProperty "FilesManifest can decode encoded Manifest" $ do
        manifest@Manifest{fileNames} <- genManifest
        let
          encoded = Aeson.encodePretty manifest
          FilesManifest fileNamesDecoded =
            fromRight (error "Failed to decode manifest") (Aeson.eitherDecode encoded)

        pure (fileNamesDecoded Q.=== fileNames)
    , testCase "domain periods retain their exact JSON representation" $ do
        let check period expectedJSON expectedPeriod = do
              let manifest = Manifest "" Nothing (0, 0) [] [] "" [] doms []
                  doms = HashMap.singleton "System" vSystem{vPeriod=period}
                  periodField = Aeson.withObject "Manifest" $ \obj -> do
                    domainMap <- obj .: "domains"
                    system <- domainMap .: "System"
                    system .: "period"
              Aeson.parseEither periodField (Aeson.toJSON manifest) @?= Right expectedJSON
              let decoded = Aeson.eitherDecode (Aeson.encode manifest)
              fmap (fmap vPeriod . domains) decoded @?=
                Right (HashMap.singleton "System" expectedPeriod)
            exactThird = Aeson.object
              ["numerator" .= (10000 :: Int), "denominator" .= (3 :: Int)]
        check 10000 (Aeson.Number 10000) 10000
        check (Period (20000 :% 2)) (Aeson.Number 10000) 10000
        check (10000 / 3) exactThird (10000 / 3)
    ]
