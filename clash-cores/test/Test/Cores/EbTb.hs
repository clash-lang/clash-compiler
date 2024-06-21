-- |
--   Copyright   :  (C) 2024, QBayLogic B.V.
--   License     :  BSD2 (see the file LICENSE)
--   Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
--
--   8b/10b encoding and decoding tests
module Test.Cores.EbTb where

import Clash.Cores.EbTb
import Clash.Hedgehog.Sized.BitVector
import qualified Clash.Prelude as C
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Prelude

-- | Check if a 'BitVector' does not contain a sequence of bits with the same
--   value for 5 or more bits consequetively.
checkBitSequence :: (C.KnownNat n) => C.BitVector n -> Bool
checkBitSequence cg =
  fst (fst (C.mapAccumL (f 0) (0, 0) (C.bv2v cg)))
    < 5
    && fst (fst (C.mapAccumL (f 1) (0, 0) (C.bv2v cg)))
      < 5
 where
  f :: C.Bit -> (Int, Int) -> C.Bit -> ((Int, Int), C.Bit)
  f c (accMax, acc) i =
    if i == c
      then ((if accMax < (acc + 1) then acc + 1 else accMax, acc + 1), i)
      else ((accMax, 0), i)

-- Check if the output of 'ebTbDecode' is a valid value for a given value from
-- 'ebTbEncode'.
prop_ebTbDecodeCheckNothing :: H.Property
prop_ebTbDecodeCheckNothing = H.property $ do
  inp <- H.forAll (Gen.filterT checkBitSequence genDefinedBitVector)
  let out = isValidDw dw1 && isValidDw dw2
       where
        (_, dw1) = ebTbDecode False $ snd $ ebTbEncode False (Dw inp)
        (_, dw2) = ebTbDecode True $ snd $ ebTbEncode True (Dw inp)

  H.assert out

-- | Encode the input signal and check whether it is a valid value. It should be
--   valid for every possible input.
prop_ebTbEncodeCheckNothing :: H.Property
prop_ebTbEncodeCheckNothing = H.property $ do
  inp <- H.forAll genDefinedBitVector
  let out = 0 /= snd (ebTbEncode False (Dw inp))

  H.assert out

-- | Encode and then decode the input signal, but if the result of the encode or
--   decode functions is invalid, propagate the input itself to the output. The
--   properties 'prop_ebTbDecodeCheckNothing' and 'prop_ebTbEncodeCheckNothing'
--   are used to assert that there are no invalid values in the outputs of these
--   functions.
prop_ebTbEncodeDecode :: H.Property
prop_ebTbEncodeDecode = H.property $ do
  inp <- H.forAll genDefinedBitVector
  let out = if isValidDw dw then fromDw dw else inp
       where
        dw = snd $ ebTbDecode False $ snd $ ebTbEncode False (Dw inp)

      expected = inp

  out H.=== expected

-- | Encode and then decode the input signal, but if the result of the encode or
--   decode functions is invalid, propagate the input itself to the output as
--   the set of valid code groups is much larger than the set of valid data
--   words. The generated list is filtered to make sure that there are no
--   accidental commas in the input list, as these are only accepted for a small
--   subset of control signals.
prop_ebTbDecodeEncode :: H.Property
prop_ebTbDecodeEncode = H.property $ do
  inp <-
    H.forAll
      (Gen.filterT checkBitSequence genDefinedBitVector)
  let out = if o == inp then o else g True inp
       where
        o = g False inp

        g rd i = if isValidDw dw then snd $ ebTbEncode rd dw else i
         where
          dw = snd $ ebTbDecode rd i

      expected = inp

  out H.=== expected

tests :: TestTree
tests = $(testGroupGenerator)
