{-|
Copyright  :  (C) 2019, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}

module Clash.Tests.Netlist.Id (
    module Clash.Tests.Netlist.Id
  ) where

import qualified Clash.Netlist.Types as Id
import qualified Clash.Netlist.Id as Id

import Clash.Annotations.Primitive
import Control.Monad.Trans.State.Lazy
import qualified Data.ByteString as BS
import Data.Coerce
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Test.QuickCheck.Utf8

newtype NonEmptyText = NonEmptyText Text deriving (Show)
newtype ArbitraryText = ArbitraryText Text deriving (Show)
newtype ArbitraryAsciiText = ArbitraryAsciiText Text deriving (Show)

instance Arbitrary ArbitraryAsciiText where
  arbitrary = coerce (decodeUtf8 . BS.concat <$> listOf oneByte)
  shrink = coerce shrinkValidUtf8

instance Arbitrary ArbitraryText where
  arbitrary = coerce genValidUtf8
  shrink = coerce shrinkValidUtf8

instance Arbitrary NonEmptyText where
  arbitrary = coerce genValidUtf81
  shrink = coerce shrinkValidUtf81

eval :: Bool -> HDL -> State Id.IdentifierSet a -> a
eval esc hdl a = evalState a (Id.emptyIdentifierSet esc False hdl)

eval' :: State Id.IdentifierSet a -> a
eval' = eval True VHDL

roundTrip :: Bool -> HDL -> Text -> Text
roundTrip esc hdl = Id.toText . eval esc hdl . Id.make

roundTrip' :: Text -> Text
roundTrip' = roundTrip True VHDL

roundTripTest :: Text -> TestTree
roundTripTest t =
  testCase (Text.unpack ("roundTrip: " <> t)) (t @=? roundTrip' t)

-- | Raw identifiers should always come up the same after 'Id.toText'
rawToIdProperty :: NonEmptyText -> Property
rawToIdProperty t = coerce t === Id.toText (eval' (Id.addRaw (coerce t)))

xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Netlist.Id"
    [ testCase "roundTrip: empty id" ("clash_internal" @=? roundTrip' "")

    -- Round trip tests tess whether a "make -> to text" roundtrip ~ id
    , roundTripTest "foo_bar"
    , roundTripTest "foo_1"
    , roundTripTest "foo_1_2"
    , roundTripTest "foo_1_2_ab"
    , roundTripTest "foo_1_ab_2"

    , testGroup "no collisions (one id)" $ flip map [minBound..maxBound] $ \hdl ->
        testProperty (show hdl) $ \id0 -> eval True hdl $ do
          id0t <- Id.toText <$> Id.make (coerce @ArbitraryAsciiText id0)
          id1t <- Id.toText <$> Id.make (coerce @ArbitraryAsciiText id0)
          pure (id0t /= id1t)

    , testGroup "no collisions (two ids)" $ flip map [minBound..maxBound] $ \hdl ->
        testProperty (show hdl) $ \id0 id1 -> eval True hdl $ do
          id0t <- Id.toText <$> Id.make (coerce @ArbitraryAsciiText id0)
          id1t <- Id.toText <$> Id.make (coerce @ArbitraryAsciiText id1)
          pure (id0t /= id1t)

    , testGroup "make0" $ eval' $ do
        id0 <- Id.toText <$> Id.make "foo"
        id1 <- Id.toText <$> Id.make "foo"
        id2 <- Id.toText <$> Id.make "foo_0"
        id3 <- Id.toText <$> Id.make "foo"
        id4 <- Id.toText <$> Id.make "foo_0"
        pure [ testCase "id0 == foo"     $ id0 @?= "foo"
             , testCase "id1 == foo_0"   $ id1 @?= "foo_0"
             , testCase "id2 == foo_0_0" $ id2 @?= "foo_0_0"
             , testCase "id3 == foo_0_1" $ id3 @?= "foo_1"
             , testCase "id4 == foo_0_0" $ id4 @?= "foo_0_1"
             ]

    , testGroup "make1" $ eval' $ do
        id0 <- Id.toText <$> Id.make "foo"
        id1 <- Id.toText <$> Id.make "foo_37"
        id2 <- Id.toText <$> Id.make "foo"
        id3 <- Id.toText <$> Id.make "foo_3"
        pure [ testCase "id0 == foo"    $ id0 @?= "foo"
             , testCase "id1 == foo_37" $ id1 @?= "foo_37"
             , testCase "id2 == foo_38" $ id2 @?= "foo_38"
             , testCase "id3 == foo_3"  $ id3 @?= "foo_3"
             ]

    -- Some tools/hdls are case insensitive, so we should make sure we are too
    , testGroup "case sensitivity" $ eval' $ do
        id0 <- Id.toText <$> Id.make "foobar"
        id1 <- Id.toText <$> Id.make "fOoBAr"
        pure [ testCase "id0 == foobar"   $ id0 @?= "foobar"
             , testCase "id1 == fOoBAr_0" $ id1 @?= "fOoBAr_0"
             ]

    -- An identifier made with 'mkBasic' should pass the 'isBasic' test
    , testGroup "mkBasic" $ concat $ flip map [minBound..maxBound] $ \hdl ->
      [ testProperty (show hdl <> " (ascii)")
          (Id.isBasic# hdl . roundTrip False hdl . coerce @ArbitraryAsciiText)
      , testProperty (show hdl <> " (UTF8)")
          (Id.isBasic# hdl . roundTrip False hdl . coerce @ArbitraryText)
      ]

      -- We expect a processed identifier to be either a valid basic xor
      -- extended identifier. Anything "in between" is an error.
    , testGroup "Basic XOR Extended" $ flip map [minBound..maxBound] $ \hdl ->
        testProperty (show hdl) $ \id0 ->
          let id1 = roundTrip True hdl (coerce @ArbitraryText id0) in
          Id.isBasic# hdl id1 `xor` Id.isExtended# hdl id1

    , testCase "keyword (use => \\use\\)" ("\\use\\" @=? roundTrip' "use")
    , testCase "keyword (else => \\else\\)" ("\\else\\" @=? roundTrip' "else")
    , testCase "keyword (record => \\record\\)" ("\\record\\" @=? roundTrip' "record")
    , testCase "keyword (configuration => \\configuration\\)" ("\\configuration\\" @=? roundTrip' "configuration")
    , testCase "keyword (cOnFiGUrAtiON => \\cOnFiGUrAtiON\\)" ("\\cOnFiGUrAtiON\\" @=? roundTrip' "cOnFiGUrAtiON")
    , testCase "Verilog keyword in VHDL (always => always)" ("always" @=? roundTrip' "always")

    , testGroup "extended identifiers"
      [ testCase "(1) foo bar => \\foo bar\\" $ "\\foo bar\\" @=? roundTrip' "foo bar"
      , testCase "(2) foo bar => \\foo bar\\" $ 9 @=? Text.length (roundTrip' "foo bar")

      , testCase "foo\\bar => foobar" $ "foobar" @=? roundTrip' "foo\\bar"
      , testCase "\\foobar\\ => foobar" $ "foobar" @=? roundTrip' "\\foobar\\"

      -- This behavior makes sense, but it results in ugly identifiers, so
      -- backslashes are stripped
      -- , testCase "foo\\bar => \\foo\\\\bar\\" $ "\\foo\\\\bar\\" @=? roundTrip' "foo\\bar"
      -- , testCase "\\foobar\\ => \\\\\\foobar\\\\\\" $ "\\\\\\foobar\\\\\\" @=? roundTrip' "\\foobar\\"
      ]

    , testGroup "pretty names"
      [ testCase "(# #) => Unit" $ "Unit" @=? roundTrip' "(# #)"
      , testCase "() => Unit" $ "Unit" @=? roundTrip' "()"
      , testCase "(,,) => Tup3" $ "Tup3" @=? roundTrip' "(,,)"
      , testCase "(#,,,,#) => Tup5" $ "Tup5" @=? roundTrip' "(,,,,)"
      ]

    , testGroup "pretty names (force basic)"
      [ testCase "(# #) => Unit" $ "Unit" @=? roundTrip False VHDL "(# #)"
      , testCase "() => Unit" $ "Unit" @=? roundTrip False VHDL "()"
      , testCase "(,,) => Tup3" $ "Tup3" @=? roundTrip False VHDL "(,,)"
      , testCase "(#,,,,#) => Tup5" $ "Tup5" @=? roundTrip False VHDL "(,,,,)"
      ]

    , testGroup "disallow escaped identifiers"
      [ testCase "foo bar => foobar" $ "foobar" @=? roundTrip False VHDL "foo bar"
      , testCase "foo\\bar => foobar" $ "foobar" @=? roundTrip False VHDL "foo\\bar"
      ]

      -- Raw identifiers are a bit weird: they're passed in by users and should
      -- be spliced into the HDL at verbatim. Clash shouldn't generate collisions
      -- though.
    , testGroup "raw identifiers"
      [ testProperty "id" rawToIdProperty
      , testGroup "Verilog: \\foo bar␣" $ eval True Verilog $ do
          id0 <- Id.toText <$> Id.addRaw "\\foo bar "
          id1 <- Id.toText <$> Id.make "foo bar"
          pure [ testCase "id0 == \\foo bar " $ id0 @?= "\\foo bar "
               , testCase "id1 == \\foo bar_0 " $ id1 @?= "\\foo bar_0 "
               ]
      , testGroup "Verilog: \\foo bar␣␣" $ eval True Verilog $ do
          id0 <- Id.toText <$> Id.addRaw "\\foo bar  "
          id1 <- Id.toText <$> Id.make "foo bar"
          pure [ testCase "id0 == \\foo bar  " $ id0 @?= "\\foo bar  "
               , testCase "id1 == \\foo bar_0 " $ id1 @?= "\\foo bar_0 "
               ]
      , testGroup "VHDL: \\foo bar\\" $ eval True VHDL $ do
          id0 <- Id.toText <$> Id.addRaw "\\foo bar\\"
          id1 <- Id.toText <$> Id.make "foo bar"
          pure [ testCase "id0 == \\foo bar\\" $ id0 @?= "\\foo bar\\"
               , testCase "id1 == \\foo bar_0\\ " $ id1 @?= "\\foo bar_0\\"
               ]
      , testGroup "VHDL: \\foo bar \\" $ eval True VHDL $ do
          id0 <- Id.toText <$> Id.addRaw "\\foo bar \\"
          id1 <- Id.toText <$> Id.make "foo bar"
          -- While 'id1' could strictly be \foo bar\, it's probably best to be
          -- whitespace insensitive.
          pure [ testCase "id0 == \\foo bar \\" $ id0 @?= "\\foo bar \\"
               , testCase "id1 == \\foo bar_0\\ " $ id1 @?= "\\foo bar_0\\"
               ]
      ]
    ]
