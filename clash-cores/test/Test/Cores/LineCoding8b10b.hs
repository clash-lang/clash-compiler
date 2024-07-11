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

-- | Encode the input signal and check whether it is a valid value
prop_encode8b10bCheckValid :: H.Property
prop_encode8b10bCheckValid = H.withTests 1000 $ H.property $ do
  inp <- H.forAll genDefinedBitVector

  snd (encode8b10b True (Dw inp)) H./== 0
  snd (encode8b10b False (Dw inp)) H./== 0

-- | Encode and then decode a valid input and check whether it is the same
prop_encodeDecode8b10b :: H.Property
prop_encodeDecode8b10b = H.withTests 1000 $ H.property $ do
  inp <- H.forAll genDefinedBitVector
  let out = case ( isValidSymbol dw1
                 , isValidSymbol dw2
                 , isValidSymbol dw3
                 , isValidSymbol dw4
                 ) of
        (True, _, _, _) -> dw1
        (_, True, _, _) -> dw2
        (_, _, True, _) -> dw3
        (_, _, _, True) -> dw4
        _ -> undefined
       where
        dw1 = snd $ decode8b10b True cg1
        dw2 = snd $ decode8b10b True cg2
        dw3 = snd $ decode8b10b False cg1
        dw4 = snd $ decode8b10b False cg2

        cg1 = snd $ encode8b10b True (Dw inp)
        cg2 = snd $ encode8b10b False (Dw inp)

      expected = Dw inp

  out H.=== expected

-- | Decode and then encode a valid input, and then decode it and the input to
--   check if they are the same
prop_decodeEncode8b10b :: H.Property
prop_decodeEncode8b10b = H.withTests 1000 $ H.property $ do
  inp <- H.forAll (Gen.filter isValidCodeGroup genDefinedBitVector)
  let out = case ( isValidSymbol dw3
                 , isValidSymbol dw4
                 , isValidSymbol dw5
                 , isValidSymbol dw6
                 ) of
        (True, _, _, _) -> dw3
        (_, True, _, _) -> dw4
        (_, _, True, _) -> dw5
        (_, _, _, True) -> dw6
        _ -> undefined
       where
        dw3 = snd $ decode8b10b True cg1
        dw4 = snd $ decode8b10b True cg2
        dw5 = snd $ decode8b10b False cg1
        dw6 = snd $ decode8b10b False cg2

        cg1 = snd $ case (isValidSymbol dw1, isValidSymbol dw2) of
          (True, _) -> encode8b10b False dw1
          (_, True) -> encode8b10b False dw2
          _ -> undefined

        cg2 = snd $ case (isValidSymbol dw1, isValidSymbol dw2) of
          (True, _) -> encode8b10b True dw1
          (_, True) -> encode8b10b True dw2
          _ -> undefined

        dw1 = snd $ decode8b10b True inp
        dw2 = snd $ decode8b10b False inp

      expected = case (isValidSymbol dw1, isValidSymbol dw2) of
        (True, _) -> dw1
        (_, True) -> dw2
        _ -> undefined
       where
        dw1 = snd $ decode8b10b True inp
        dw2 = snd $ decode8b10b False inp

  out H.=== expected

tests :: TestTree
tests = $(testGroupGenerator)
