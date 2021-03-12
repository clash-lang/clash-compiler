module Clash.Tests.Driver.Manifest where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Coerce (coerce)
import Data.Text (Text)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Test.Tasty

import qualified Test.Tasty.QuickCheck as Q
import qualified Test.QuickCheck.Utf8 as Q

import Clash.Driver.Manifest
import Clash.Explicit.Signal

import Debug.Trace

newtype ArbitraryText = ArbitraryText Text deriving (Show)
newtype ArbitraryManifest = ArbitraryManifest Manifest deriving (Show)

instance Q.Arbitrary ArbitraryText where
  arbitrary = coerce Q.genValidUtf8
  shrink = coerce Q.shrinkValidUtf8

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
    <*> (fromIntegral @Int . abs <$> Q.arbitraryBoundedIntegral)
    <*> Q.elements [False, True]
    <*> coerce @(Q.Gen (Maybe ArbitraryText)) Q.arbitrary

genManifest :: Q.Gen Manifest
genManifest =
  Manifest
    <$> Q.arbitrary -- hash
    <*> Q.arbitrary -- flags
    <*> Q.listOf genPort -- in ports
    <*> Q.listOf genPort -- out ports
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

        if decoded == Right manifest then
          pure True
        else do
          !_ <- traceM "-------------------------"
          !_ <- traceM (show manifest)
          !_ <- traceM (Text.unpack (Text.decodeUtf8 (LazyByteString.toStrict encoded)))
          !_ <- traceM (show decoded)
          pure False
    ]
