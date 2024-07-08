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
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Prelude

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

-- | Function that checks whether a 'BitVector' is a valid data word
codeGroupOk :: C.BitVector 10 -> Bool
codeGroupOk cg = isDw $ snd $ decode8b10b True cg

-- Check if the output of 'decode8b10b' is a valid value for a given value from
-- 'encode8b10b'.
prop_decode8b10bCheckNothing :: H.Property
prop_decode8b10bCheckNothing = H.property $ do
  inp <- H.forAll genDefinedBitVector
  let out = isValidSymbol dw1 && isValidSymbol dw2
       where
        (_, dw1) = decode8b10b False $ snd $ encode8b10b False (Dw inp)
        (_, dw2) = decode8b10b True $ snd $ encode8b10b True (Dw inp)

  H.assert out

-- | Encode the input signal and check whether it is a valid value. It should be
--   valid for every possible input.
prop_encode8b10bCheckNothing :: H.Property
prop_encode8b10bCheckNothing = H.property $ do
  inp <- H.forAll genDefinedBitVector
  let out = 0 /= snd (encode8b10b False (Dw inp))

  H.assert out

-- | Encode and then decode the input signal, but if the result of the encode or
--   decode functions is invalid, propagate the input itself to the output. The
--   properties 'prop_decode8b10bCheckNothing' and
--   'prop_encode8b10bCheckNothing' are used to assert that there are no invalid
--   values in the outputs of these functions.
prop_encodeDecode8b10b :: H.Property
prop_encodeDecode8b10b = H.property $ do
  inp <- H.forAll genDefinedBitVector
  let out = if isValidSymbol dw then fromDw dw else inp
       where
        dw = snd $ decode8b10b False $ snd $ encode8b10b False (Dw inp)

      expected = inp

  out H.=== expected

-- | Encode and then decode the input signal, but if the result of the encode or
--   decode functions is invalid, propagate the input itself to the output as
--   the set of valid code groups is much larger than the set of valid data
--   words. The generated list is filtered to make sure that there are no
--   accidental commas in the input list, as these are only accepted for a small
--   subset of control signals.
prop_decodeEncode8b10b :: H.Property
prop_decodeEncode8b10b = H.property $ do
  inp <-
    H.forAll
      (Gen.filter codeGroupOk genDefinedBitVector)
  let out = if o == inp then o else g True inp
       where
        o = g False inp

        g rd i = if isValidSymbol dw then snd $ encode8b10b rd dw else i
         where
          dw = snd $ decode8b10b rd i

      expected = inp

  out H.=== expected

tests :: TestTree
tests = $(testGroupGenerator)
