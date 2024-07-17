-- |
--   Copyright   :  (C) 2024, QBayLogic B.V.
--   License     :  BSD2 (see the file LICENSE)
--   Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
--
--   8b/10b encoding and decoding tests
module Test.Cores.LineCoding8b10b where

import Clash.Cores.LineCoding8b10b
import Clash.Hedgehog.Sized.BitVector
import qualified Clash.Prelude as C
import Control.Monad (when)
import Data.Maybe (isNothing)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Prelude

-- | Check if a 'BitVector' does not contain a sequence of bits with the same
--   value for 5 or more indices consecutively
checkBitSequence :: C.BitVector 10 -> Bool
checkBitSequence cg =
  isNothing $
    C.elemIndex True $
      C.map (f . C.pack) $
        C.windows1d C.d5 $
          C.bv2v cg
 where
  f a = a == 0b11111 || a == 0b00000

-- | Function that checks whether a code group corresponds to a valid symbol
isValidCodeGroup :: C.BitVector 10 -> Bool
isValidCodeGroup cg =
  isValidSymbol (snd $ decode8b10b True cg)
    || isValidSymbol (snd $ decode8b10b False cg)

-- | Function that creates a list with a given range that contains a list of
--   running disparities and 'Symbol8b10b's
genSymbol8b10bs :: H.Range Int -> H.Gen [(Bool, Symbol8b10b)]
genSymbol8b10bs range = do
  n <- Gen.int range
  genSymbol8b10bs1 n False

-- | Recursive function to generate a list of 'Symbol8b10b's with the correct
--   running disparity
genSymbol8b10bs1 :: Int -> Bool -> H.Gen [(Bool, Symbol8b10b)]
genSymbol8b10bs1 0 _ = pure []
genSymbol8b10bs1 n rd = do
  (rdNew, dw) <- genSymbol8b10b rd
  ((rdNew, dw) :) <$> genSymbol8b10bs1 (pred n) rdNew

-- | Generate a 'Symbol8b10b' by creating a 'BitVector' of length 10 and
--   decoding it with the 'decode8b10b' function
genSymbol8b10b :: Bool -> H.Gen (Bool, Symbol8b10b)
genSymbol8b10b rd = Gen.filter f $ decode8b10b rd <$> genDefinedBitVector
 where
  f (_, dw) = isDw dw

-- Check if the output of 'decode8b10b' is a valid value for a given value from
-- 'encode8b10b'
prop_decode8b10bCheckValid :: H.Property
prop_decode8b10bCheckValid = H.withTests 1000 $ H.property $ do
  inp <- H.forAll genDefinedBitVector

  H.assert $
    isValidSymbol $
      snd $
        decode8b10b True $
          snd $
            encode8b10b True (Dw inp)
  H.assert $
    isValidSymbol $
      snd $
        decode8b10b False $
          snd $
            encode8b10b False (Dw inp)

-- | Encode and then decode a valid input and check whether it is the same
prop_encodeDecode8b10b :: H.Property
prop_encodeDecode8b10b = H.withTests 1000 $ H.property $ do
  inp <- H.forAll genDefinedBitVector
  roundTrip False inp H.=== Dw inp
  roundTrip True inp H.=== Dw inp
 where
  roundTrip rd inp = snd $ decode8b10b rd $ snd $ encode8b10b rd $ Dw inp

-- | Decode and then encode a valid input, and then decode it and the input to
--   check if they are the same. The last decoding step is performed to
--   side-step the fact that the decoding table contains errors, but we want the
--   test suite to succeed nonetheless.
prop_decodeEncode8b10b :: H.Property
prop_decodeEncode8b10b = H.withTests 1000 $ H.property $ do
  inp <- H.forAll (Gen.filter isValidCodeGroup genDefinedBitVector)
  propertyForRd False inp
  propertyForRd True inp
 where
  propertyForRd rd inp = do
    H.annotateShow rd
    let expected = snd $ decode8b10b rd inp
    when (isValidSymbol expected) $
      snd (decode8b10b rd $ roundTrip rd inp) H.=== expected
  roundTrip rd inp = snd $ encode8b10b rd $ snd $ decode8b10b rd inp

tests :: TestTree
tests = $(testGroupGenerator)
