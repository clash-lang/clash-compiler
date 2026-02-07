{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}

{- |
Copyright  :  (C) 2025     , Martijn Bastiaan
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Test generation of 'NumConvert' instances:

> constraints = {
>     ("Index",     "Index")     : (False, "SNat @{n}              `compareSNat` SNat @{m}"),
>     ("Index",     "Unsigned")  : (False, "SNat @{n}              `compareSNat` SNat @(2 ^ {m})"),
>     ("Index",     "Signed")    : (True,  "SNat @(CLog 2 {n} + 1) `compareSNat` SNat @{m}"),
>     ("Index",     "BitVector") : (False, "SNat @{n}              `compareSNat` SNat @(2 ^ {m})"),
>     ("Unsigned",  "Index")     : (False, "SNat @(2^{n})          `compareSNat` SNat @{m}"),
>     ("Unsigned",  "Unsigned")  : (False, "SNat @{n}              `compareSNat` SNat @{m}"),
>     ("Unsigned",  "Signed")    : (True,  "SNat @({n} + 1)        `compareSNat` SNat @{m}"),
>     ("Unsigned",  "BitVector") : (False, "SNat @{n}              `compareSNat` SNat @{m}"),
>     ("Signed",    "Signed")    : (False, "SNat @{n}              `compareSNat` SNat @{m}"),
>     ("BitVector", "Index")     : (False, "SNat @(2^{n})          `compareSNat` SNat @{m}"),
>     ("BitVector", "Unsigned")  : (False, "SNat @{n}              `compareSNat` SNat @{m}"),
>     ("BitVector", "Signed")    : (True,  "SNat @({n} + 1)        `compareSNat` SNat @{m}"),
>     ("BitVector", "BitVector") : (False, "SNat @{n}              `compareSNat` SNat @{m}"),
> }
>
> for a in ["Index", "Unsigned", "Signed", "BitVector"]:
>     for b in ["Index", "Unsigned", "Signed", "BitVector"]:
>         ia_max = "indexMax" if a == "Index" else "otherMax"
>         ib_max = "indexMax" if b == "Index" else "otherMax"
>         n = "(n + 1)" if a == "Index" else "n"
>         m = "(m + 1)" if b == "Index" else "m"
>         if (a, b) not in constraints:
>             continue
>         print(f"""case_convert{a}{b} :: Assertion
> case_convert{a}{b} =
>   forM_ [0 .. {ia_max}] $ \\n ->
>     forM_ [0 .. {ib_max}] $ \\m ->
>       withSomeSNat n $ \\(SNat :: SNat n) ->
>         withSomeSNat m $ \\(SNat :: SNat m) ->
>           case {constraints[(a, b)][1].format(n=n, m=m)} of
>             SNatLE -> do
>               assertBool (show (n, m)) (convertXException (Proxy @({a} {n})) (Proxy @({b} {m})))
>               forM_ [minBound .. maxBound] $ \\(i :: {a} {n}) -> do
>                 assertBool (show (n, m, i)) (convertLaw1 (Proxy @({b} {m})) i)
>                 assertBool (show (n, m, i)) (convertLaw2 (Proxy @({b} {m})) i)
>             _ | (n == 0 && m == 0 && zeroWidthSkip) -> pure ()
>             _ -> assertBool (show (n, m)) (convertLaw3 (Proxy @({a} {n})) (Proxy @({b} {m})))
>  where
>   zeroWidthSkip = {constraints[(a, b)][0]}
>  """)
-}
module Clash.Tests.NumConvert where

import Control.Monad (forM_)
import Data.Data (Proxy (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Maybe (isNothing)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeNats (someNatVal)
import Test.Tasty (TestTree, defaultMain)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Test.Tasty.TH (testGroupGenerator)

import qualified Data.List as L

import Clash.Prelude hiding (someNatVal, withSomeSNat)

convertLaw1 :: forall a b. (NumConvert a b, MaybeNumConvert b a, Eq a) => Proxy b -> a -> Bool
convertLaw1 _ x = Just x == maybeNumConvert (numConvert @a @b x)

convertLaw2 :: forall a b. (NumConvert a b, Eq a, Integral b, Integral a) => Proxy b -> a -> Bool
convertLaw2 _ x = toInteger x == toInteger (numConvert @a @b x)

{- | Tightness law: this law is tested for if there is _no_ instance of
'NumConvert'. If this is the case, 'MaybeNumConvert' should at least return a 'Nothing'
once when converting the domain of @a@ to @b@. If all conversions are possible,
the constraints of the instances should be relaxed. If the domain of @a@ is
empty, this law is considered satisfied too.
-}
convertLaw3 :: forall a b. (MaybeNumConvert a b, Bounded a, Enum a) => Proxy a -> Proxy b -> Bool
convertLaw3 _ _ = L.any isNothing results
 where
  results = L.map (maybeNumConvert @a @b) [minBound ..]

-- | Checks whether an 'XException' in, means an 'XException' out
convertXException :: forall a b. (NumConvert a b) => Proxy a -> Proxy b -> Bool
convertXException _ _ = case isX $ numConvert @a @b (errorX "BOO" :: a) of
  Left s -> "BOO" `L.isInfixOf` s
  Right _ -> False

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = $(testGroupGenerator)

-- Tests for numConvertVia (Word-to-Word conversions)

case_throughWord8Word16 :: Assertion
case_throughWord8Word16 = do
  assertBool "0" $ numConvertVia (0 :: Word8) == (0 :: Word16)
  assertBool "1" $ numConvertVia (1 :: Word8) == (1 :: Word16)
  assertBool "42" $ numConvertVia (42 :: Word8) == (42 :: Word16)
  assertBool "maxBound" $ numConvertVia (maxBound :: Word8) == (255 :: Word16)

case_throughWord8Word32 :: Assertion
case_throughWord8Word32 = do
  assertBool "0" $ numConvertVia (0 :: Word8) == (0 :: Word32)
  assertBool "1" $ numConvertVia (1 :: Word8) == (1 :: Word32)
  assertBool "42" $ numConvertVia (42 :: Word8) == (42 :: Word32)
  assertBool "maxBound" $ numConvertVia (maxBound :: Word8) == (255 :: Word32)

case_throughWord16Word32 :: Assertion
case_throughWord16Word32 = do
  assertBool "0" $ numConvertVia (0 :: Word16) == (0 :: Word32)
  assertBool "1" $ numConvertVia (1 :: Word16) == (1 :: Word32)
  assertBool "42" $ numConvertVia (42 :: Word16) == (42 :: Word32)
  assertBool "maxBound" $ numConvertVia (maxBound :: Word16) == (65535 :: Word32)

case_throughWord32Word64 :: Assertion
case_throughWord32Word64 = do
  assertBool "0" $ numConvertVia (0 :: Word32) == (0 :: Word64)
  assertBool "1" $ numConvertVia (1 :: Word32) == (1 :: Word64)
  assertBool "42" $ numConvertVia (42 :: Word32) == (42 :: Word64)
  assertBool "maxBound" $ numConvertVia (maxBound :: Word32) == (4294967295 :: Word64)

-- Tests for numConvertVia (Int-to-Int conversions)

case_throughInt8Int16 :: Assertion
case_throughInt8Int16 = do
  assertBool "0" $ numConvertVia (0 :: Int8) == (0 :: Int16)
  assertBool "1" $ numConvertVia (1 :: Int8) == (1 :: Int16)
  assertBool "42" $ numConvertVia (42 :: Int8) == (42 :: Int16)
  assertBool "-1" $ numConvertVia (-1 :: Int8) == (-1 :: Int16)
  assertBool "-42" $ numConvertVia (-42 :: Int8) == (-42 :: Int16)
  assertBool "minBound" $ numConvertVia (minBound :: Int8) == (-128 :: Int16)
  assertBool "maxBound" $ numConvertVia (maxBound :: Int8) == (127 :: Int16)

case_throughInt32Int64 :: Assertion
case_throughInt32Int64 = do
  assertBool "0" $ numConvertVia (0 :: Int32) == (0 :: Int64)
  assertBool "1" $ numConvertVia (1 :: Int32) == (1 :: Int64)
  assertBool "42" $ numConvertVia (42 :: Int32) == (42 :: Int64)
  assertBool "-1" $ numConvertVia (-1 :: Int32) == (-1 :: Int64)
  assertBool "-42" $ numConvertVia (-42 :: Int32) == (-42 :: Int64)
  assertBool "minBound" $ numConvertVia (minBound :: Int32) == (-2147483648 :: Int64)
  assertBool "maxBound" $ numConvertVia (maxBound :: Int32) == (2147483647 :: Int64)

-- Tests for mixed conversions (cross Haskell/Clash boundary)

case_throughWord32Unsigned64 :: Assertion
case_throughWord32Unsigned64 = do
  assertBool "0" $ numConvertVia (0 :: Word32) == (0 :: Unsigned 64)
  assertBool "1" $ numConvertVia (1 :: Word32) == (1 :: Unsigned 64)
  assertBool "42" $ numConvertVia (42 :: Word32) == (42 :: Unsigned 64)
  assertBool "maxBound" $ numConvertVia (maxBound :: Word32) == (4294967295 :: Unsigned 64)

case_throughUnsigned32Word64 :: Assertion
case_throughUnsigned32Word64 = do
  assertBool "0" $ numConvertVia (0 :: Unsigned 32) == (0 :: Word64)
  assertBool "1" $ numConvertVia (1 :: Unsigned 32) == (1 :: Word64)
  assertBool "42" $ numConvertVia (42 :: Unsigned 32) == (42 :: Word64)
  assertBool "maxBound" $ numConvertVia (maxBound :: Unsigned 32) == (4294967295 :: Word64)

withSomeSNat :: Natural -> (forall (n :: Nat). SNat n -> r) -> r
withSomeSNat n f = case someNatVal n of
  SomeNat (_ :: Proxy n) -> f (SNat @n)

indexMax :: Natural
indexMax = 128

otherMax :: Natural
otherMax = 8

case_convertIndexIndex :: Assertion
case_convertIndexIndex =
  forM_ [0 .. indexMax] $ \n ->
    forM_ [0 .. indexMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @(n + 1) `compareSNat` SNat @(m + 1) of
            SNatLE -> do
              assertBool (show (n, m)) (convertXException (Proxy @(Index (n + 1))) (Proxy @(Index (m + 1))))
              forM_ [minBound .. maxBound] $ \(i :: Index (n + 1)) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(Index (m + 1))) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(Index (m + 1))) i)
            _ | (n == 0 && m == 0 && zeroWidthSkip) -> pure ()
            _ -> assertBool (show (n, m)) (convertLaw3 (Proxy @(Index (n + 1))) (Proxy @(Index (m + 1))))
 where
  zeroWidthSkip = False

case_convertIndexUnsigned :: Assertion
case_convertIndexUnsigned =
  forM_ [0 .. indexMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @(n + 1) `compareSNat` SNat @(2 ^ m) of
            SNatLE -> do
              assertBool (show (n, m)) (convertXException (Proxy @(Index (n + 1))) (Proxy @(Unsigned m)))
              forM_ [minBound .. maxBound] $ \(i :: Index (n + 1)) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(Unsigned m)) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(Unsigned m)) i)
            _ | (n == 0 && m == 0 && zeroWidthSkip) -> pure ()
            _ -> assertBool (show (n, m)) (convertLaw3 (Proxy @(Index (n + 1))) (Proxy @(Unsigned m)))
 where
  zeroWidthSkip = False

case_convertIndexSigned :: Assertion
case_convertIndexSigned =
  forM_ [0 .. indexMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @(CLog 2 (n + 1) + 1) `compareSNat` SNat @m of
            SNatLE -> do
              assertBool (show (n, m)) (convertXException (Proxy @(Index (n + 1))) (Proxy @(Signed m)))
              forM_ [minBound .. maxBound] $ \(i :: Index (n + 1)) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(Signed m)) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(Signed m)) i)
            _ | (n == 0 && m == 0 && zeroWidthSkip) -> pure ()
            _ -> assertBool (show (n, m)) (convertLaw3 (Proxy @(Index (n + 1))) (Proxy @(Signed m)))
 where
  zeroWidthSkip = True

case_convertIndexBitVector :: Assertion
case_convertIndexBitVector =
  forM_ [0 .. indexMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @(n + 1) `compareSNat` SNat @(2 ^ m) of
            SNatLE -> do
              assertBool (show (n, m)) (convertXException (Proxy @(Index (n + 1))) (Proxy @(BitVector m)))
              forM_ [minBound .. maxBound] $ \(i :: Index (n + 1)) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(BitVector m)) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(BitVector m)) i)
            _ | (n == 0 && m == 0 && zeroWidthSkip) -> pure ()
            _ -> assertBool (show (n, m)) (convertLaw3 (Proxy @(Index (n + 1))) (Proxy @(BitVector m)))
 where
  zeroWidthSkip = False

case_convertUnsignedIndex :: Assertion
case_convertUnsignedIndex =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. indexMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @(2 ^ n) `compareSNat` SNat @(m + 1) of
            SNatLE -> do
              assertBool (show (n, m)) (convertXException (Proxy @(Unsigned n)) (Proxy @(Index (m + 1))))
              forM_ [minBound .. maxBound] $ \(i :: Unsigned n) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(Index (m + 1))) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(Index (m + 1))) i)
            _ | (n == 0 && m == 0 && zeroWidthSkip) -> pure ()
            _ -> assertBool (show (n, m)) (convertLaw3 (Proxy @(Unsigned n)) (Proxy @(Index (m + 1))))
 where
  zeroWidthSkip = False

case_convertUnsignedUnsigned :: Assertion
case_convertUnsignedUnsigned =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @n `compareSNat` SNat @m of
            SNatLE -> do
              assertBool (show (n, m)) (convertXException (Proxy @(Unsigned n)) (Proxy @(Unsigned m)))
              forM_ [minBound .. maxBound] $ \(i :: Unsigned n) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(Unsigned m)) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(Unsigned m)) i)
            _ | (n == 0 && m == 0 && zeroWidthSkip) -> pure ()
            _ -> assertBool (show (n, m)) (convertLaw3 (Proxy @(Unsigned n)) (Proxy @(Unsigned m)))
 where
  zeroWidthSkip = False

case_convertUnsignedSigned :: Assertion
case_convertUnsignedSigned =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @(n + 1) `compareSNat` SNat @m of
            SNatLE -> do
              assertBool (show (n, m)) (convertXException (Proxy @(Unsigned n)) (Proxy @(Signed m)))
              forM_ [minBound .. maxBound] $ \(i :: Unsigned n) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(Signed m)) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(Signed m)) i)
            _ | (n == 0 && m == 0 && zeroWidthSkip) -> pure ()
            _ -> assertBool (show (n, m)) (convertLaw3 (Proxy @(Unsigned n)) (Proxy @(Signed m)))
 where
  zeroWidthSkip = True

case_convertUnsignedBitVector :: Assertion
case_convertUnsignedBitVector =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @n `compareSNat` SNat @m of
            SNatLE -> do
              assertBool (show (n, m)) (convertXException (Proxy @(Unsigned n)) (Proxy @(BitVector m)))
              forM_ [minBound .. maxBound] $ \(i :: Unsigned n) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(BitVector m)) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(BitVector m)) i)
            _ | (n == 0 && m == 0 && zeroWidthSkip) -> pure ()
            _ -> assertBool (show (n, m)) (convertLaw3 (Proxy @(Unsigned n)) (Proxy @(BitVector m)))
 where
  zeroWidthSkip = False

case_convertSignedSigned :: Assertion
case_convertSignedSigned =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @n `compareSNat` SNat @m of
            SNatLE -> do
              assertBool (show (n, m)) (convertXException (Proxy @(Signed n)) (Proxy @(Signed m)))
              forM_ [minBound .. maxBound] $ \(i :: Signed n) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(Signed m)) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(Signed m)) i)
            _ | (n == 0 && m == 0 && zeroWidthSkip) -> pure ()
            _ -> assertBool (show (n, m)) (convertLaw3 (Proxy @(Signed n)) (Proxy @(Signed m)))
 where
  zeroWidthSkip = False

case_convertBitVectorIndex :: Assertion
case_convertBitVectorIndex =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. indexMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @(2 ^ n) `compareSNat` SNat @(m + 1) of
            SNatLE -> do
              assertBool (show (n, m)) (convertXException (Proxy @(BitVector n)) (Proxy @(Index (m + 1))))
              forM_ [minBound .. maxBound] $ \(i :: BitVector n) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(Index (m + 1))) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(Index (m + 1))) i)
            _ | (n == 0 && m == 0 && zeroWidthSkip) -> pure ()
            _ -> assertBool (show (n, m)) (convertLaw3 (Proxy @(BitVector n)) (Proxy @(Index (m + 1))))
 where
  zeroWidthSkip = False

case_convertBitVectorUnsigned :: Assertion
case_convertBitVectorUnsigned =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @n `compareSNat` SNat @m of
            SNatLE -> do
              assertBool (show (n, m)) (convertXException (Proxy @(BitVector n)) (Proxy @(Unsigned m)))
              forM_ [minBound .. maxBound] $ \(i :: BitVector n) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(Unsigned m)) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(Unsigned m)) i)
            _ | (n == 0 && m == 0 && zeroWidthSkip) -> pure ()
            _ -> assertBool (show (n, m)) (convertLaw3 (Proxy @(BitVector n)) (Proxy @(Unsigned m)))
 where
  zeroWidthSkip = False

case_convertBitVectorSigned :: Assertion
case_convertBitVectorSigned =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @(n + 1) `compareSNat` SNat @m of
            SNatLE -> do
              assertBool (show (n, m)) (convertXException (Proxy @(BitVector n)) (Proxy @(Signed m)))
              forM_ [minBound .. maxBound] $ \(i :: BitVector n) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(Signed m)) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(Signed m)) i)
            _ | (n == 0 && m == 0 && zeroWidthSkip) -> pure ()
            _ -> assertBool (show (n, m)) (convertLaw3 (Proxy @(BitVector n)) (Proxy @(Signed m)))
 where
  zeroWidthSkip = True

case_convertBitVectorBitVector :: Assertion
case_convertBitVectorBitVector =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @n `compareSNat` SNat @m of
            SNatLE -> do
              assertBool (show (n, m)) (convertXException (Proxy @(BitVector n)) (Proxy @(BitVector m)))
              forM_ [minBound .. maxBound] $ \(i :: BitVector n) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(BitVector m)) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(BitVector m)) i)
            _ | (n == 0 && m == 0 && zeroWidthSkip) -> pure ()
            _ -> assertBool (show (n, m)) (convertLaw3 (Proxy @(BitVector n)) (Proxy @(BitVector m)))
 where
  zeroWidthSkip = False
