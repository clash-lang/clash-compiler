module Clash.Tests.BlockRam.Blob where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Functor.Identity
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Numeric.Natural
import Test.Tasty
import Test.Tasty.Hedgehog

import Clash.Explicit.BlockRam.Internal (packAsNats, unpackNats)

roundTripProperty :: Property
roundTripProperty = property $ do
  len <- forAll $ Gen.integral $ Range.linear 0 256
  width <- forAll $ Gen.integral $ Range.linear 1 128
  es <- forAll $ Gen.list (Range.singleton len) $
    Gen.integral_ $ Range.constant 0 (2 ^ width - 1)
  tripping (len, width, es) encode decode
 where
  encode :: (Int, Int, [Natural]) -> (Int, Int, B.ByteString, B.ByteString)
  encode (len, width, es) = let (runs, ends) = packAsNats width id es
                            in (len, width, L.toStrict runs, L.toStrict ends)
  decode :: (Int, Int, B.ByteString, B.ByteString)
         -> Identity (Int, Int, [Natural])
  decode (len, width, runs, ends) =
    let es = take 300 $ unpackNats len width runs ends
    in Identity (len, width, es)

tests :: TestTree
tests = testGroup "BlockRam"
  [ testGroup "Blob"
    [ testProperty "Round trip" roundTripProperty ]
  ]
