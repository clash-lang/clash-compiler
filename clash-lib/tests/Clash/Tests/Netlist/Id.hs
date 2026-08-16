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
import qualified Data.List as List
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

eval :: Bool -> HDL -> State Id.IdentifierScopes a -> a
eval esc hdl a = evalState a (Id.emptyIdentifierScopes esc Id.PreserveCase hdl)

eval' :: State Id.IdentifierScopes a -> a
eval' = eval True VHDL

emptyScopes :: Id.IdentifierScopes
emptyScopes = Id.emptyIdentifierScopes True Id.PreserveCase VHDL

roundTrip :: Bool -> HDL -> Text -> Text
roundTrip esc hdl = Id.toText . eval esc hdl . Id.make Id.Local

roundTrip' :: Text -> Text
roundTrip' = roundTrip True VHDL

roundTripTest :: Text -> TestTree
roundTripTest t =
  testCase (Text.unpack ("roundTrip: " <> t)) (t @=? roundTrip' t)

-- | Raw identifiers should always come up the same after 'Id.toText'
rawToIdProperty :: NonEmptyText -> Property
rawToIdProperty t = coerce t === Id.toText (eval' (Id.addRaw Id.Local (coerce t)))

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
          id0t <- Id.toText <$> Id.make Id.Local (coerce @ArbitraryAsciiText id0)
          id1t <- Id.toText <$> Id.make Id.Local (coerce @ArbitraryAsciiText id0)
          pure (id0t /= id1t)

    , testGroup "no collisions (two ids)" $ flip map [minBound..maxBound] $ \hdl ->
        testProperty (show hdl) $ \id0 id1 -> eval True hdl $ do
          id0t <- Id.toText <$> Id.make Id.Local (coerce @ArbitraryAsciiText id0)
          id1t <- Id.toText <$> Id.make Id.Local (coerce @ArbitraryAsciiText id1)
          pure (id0t /= id1t)

    , testGroup "make0" $ eval' $ do
        id0 <- Id.toText <$> Id.make Id.Local "foo"
        id1 <- Id.toText <$> Id.make Id.Local "foo"
        id2 <- Id.toText <$> Id.make Id.Local "foo_0"
        id3 <- Id.toText <$> Id.make Id.Local "foo"
        id4 <- Id.toText <$> Id.make Id.Local "foo_0"
        pure [ testCase "id0 == foo"     $ id0 @?= "foo"
             , testCase "id1 == foo_0"   $ id1 @?= "foo_0"
             , testCase "id2 == foo_0_0" $ id2 @?= "foo_0_0"
             , testCase "id3 == foo_0_1" $ id3 @?= "foo_1"
             , testCase "id4 == foo_0_0" $ id4 @?= "foo_0_1"
             ]

    , testGroup "make1" $ eval' $ do
        id0 <- Id.toText <$> Id.make Id.Local "foo"
        id1 <- Id.toText <$> Id.make Id.Local "foo_37"
        id2 <- Id.toText <$> Id.make Id.Local "foo"
        id3 <- Id.toText <$> Id.make Id.Local "foo_3"
        pure [ testCase "id0 == foo"    $ id0 @?= "foo"
             , testCase "id1 == foo_37" $ id1 @?= "foo_37"
             , testCase "id2 == foo_38" $ id2 @?= "foo_38"
             , testCase "id3 == foo_3"  $ id3 @?= "foo_3"
             ]

    , testGroup "Id.add" $ eval' $ do
        old <- get
        id0 <- Id.addRaw Id.Local "LED"
        put old
        Id.add Id.Local id0
        id1 <- Id.toText <$> Id.make Id.Local "led"
        pure [ testCase "id1 == led_0" $ id1 @?= "led_0" ]

    -- Test that names in either scope conflict with new names in the other,
    -- that each name is only registered in the scope it was made in, and that
    -- global names survive 'setLocalScope' while local names do not.
    , testGroup "scopes" $ eval' $ do
        glob0 <- Id.toText <$> Id.makeBasic Id.Global "top"
        loc0  <- Id.toText <$> Id.make Id.Local "foo"
        glob1 <- Id.toText <$> Id.makeBasic Id.Global "foo"
        loc5  <- Id.toText <$> Id.make Id.Local "top"
        globNames <- gets (List.sort . map Id.toText . Id.toList . Id._globalIds)
        locNames  <- gets (List.sort . map Id.toText . Id.toList . Id._localIds)
        let seed = Id._localIds (flip execState emptyScopes (Id.addRaw Id.Local "bar"))
        modify (Id.setLocalScope seed)
        loc1  <- Id.toText <$> Id.make Id.Local "foo"
        loc2  <- Id.toText <$> Id.make Id.Local "top"
        loc3  <- Id.toText <$> Id.make Id.Local "foo_0"
        loc4  <- Id.toText <$> Id.make Id.Local "bar"
        pure [ testCase "glob0 == top"    $ glob0 @?= "top"
             , testCase "loc0 == foo"     $ loc0 @?= "foo"
             , testCase "glob1 == foo_0"  $ glob1 @?= "foo_0"
             , testCase "loc5 == top_0"   $ loc5 @?= "top_0"
             , testCase "global scope only holds globals" $
                 globNames @?= ["foo_0", "top"]
             , testCase "local scope only holds locals" $
                 locNames @?= ["foo", "top_0"]
             , testCase "loc1 == foo"     $ loc1 @?= "foo"
             , testCase "loc2 == top_0"   $ loc2 @?= "top_0"
             , testCase "loc3 == foo_0_0" $ loc3 @?= "foo_0_0"
             , testCase "loc4 == bar_0"   $ loc4 @?= "bar_0"
             ]

    -- Some tools/hdls are case insensitive, so we should make sure we are too
    , testGroup "case sensitivity" $ eval' $ do
        id0 <- Id.toText <$> Id.make Id.Local "foobar"
        id1 <- Id.toText <$> Id.make Id.Local "fOoBAr"
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
      , testCase "(,,) => Tuple3" $ "Tuple3" @=? roundTrip' "(,,)"
      , testCase "(#,,,,#) => Tuple5" $ "Tuple5" @=? roundTrip' "(,,,,)"
      ]

    , testGroup "pretty names (force basic)"
      [ testCase "(# #) => Unit" $ "Unit" @=? roundTrip False VHDL "(# #)"
      , testCase "() => Unit" $ "Unit" @=? roundTrip False VHDL "()"
      , testCase "(,,) => Tuple3" $ "Tuple3" @=? roundTrip False VHDL "(,,)"
      , testCase "(#,,,,#) => Tuple5" $ "Tuple5" @=? roundTrip False VHDL "(,,,,)"
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
          id0 <- Id.toText <$> Id.addRaw Id.Local "\\foo bar "
          id1 <- Id.toText <$> Id.make Id.Local "foo bar"
          pure [ testCase "id0 == \\foo bar " $ id0 @?= "\\foo bar "
               , testCase "id1 == \\foo bar_0 " $ id1 @?= "\\foo bar_0 "
               ]
      , testGroup "Verilog: \\foo bar␣␣" $ eval True Verilog $ do
          id0 <- Id.toText <$> Id.addRaw Id.Local "\\foo bar  "
          id1 <- Id.toText <$> Id.make Id.Local "foo bar"
          pure [ testCase "id0 == \\foo bar  " $ id0 @?= "\\foo bar  "
               , testCase "id1 == \\foo bar_0 " $ id1 @?= "\\foo bar_0 "
               ]
      , testGroup "VHDL: \\foo bar\\" $ eval True VHDL $ do
          id0 <- Id.toText <$> Id.addRaw Id.Local "\\foo bar\\"
          id1 <- Id.toText <$> Id.make Id.Local "foo bar"
          pure [ testCase "id0 == \\foo bar\\" $ id0 @?= "\\foo bar\\"
               , testCase "id1 == \\foo bar_0\\ " $ id1 @?= "\\foo bar_0\\"
               ]
      , testGroup "VHDL: \\foo bar \\" $ eval True VHDL $ do
          id0 <- Id.toText <$> Id.addRaw Id.Local "\\foo bar \\"
          id1 <- Id.toText <$> Id.make Id.Local "foo bar"
          -- While 'id1' could strictly be \foo bar\, it's probably best to be
          -- whitespace insensitive.
          pure [ testCase "id0 == \\foo bar \\" $ id0 @?= "\\foo bar \\"
               , testCase "id1 == \\foo bar_0\\ " $ id1 @?= "\\foo bar_0\\"
               ]
      ]
    ]
