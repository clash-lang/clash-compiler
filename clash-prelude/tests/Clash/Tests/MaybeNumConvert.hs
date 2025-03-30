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

Test generation of 'MaybeNumConvert' instances:

> for a in ["Index", "Unsigned", "Signed", "BitVector"]:
>     for b in ["Index", "Unsigned", "Signed", "BitVector"]:
>          ia_max = "indexMax" if a == "Index" else "otherMax"
>          ib_max = "indexMax" if b == "Index" else "otherMax"
>          n = "(n + 1)" if a == "Index" else "n"
>          m = "(m + 1)" if b == "Index" else "m"
>          print(f"""case_maybeConvert{a}{b} :: Assertion
> case_maybeConvert{a}{b} =
>   forM_ [0 .. {ia_max}] $ \\n ->
>     forM_ [0 .. {ib_max}] $ \\m ->
>       withSomeSNat n $ \(SNat :: SNat n) ->
>         withSomeSNat m $ \(SNat :: SNat m) ->
>           forM_ [minBound .. maxBound] $ \(i :: {a} {n}) -> do
>             assertBool (show (n, m, i)) (maybeConvertLaw1 (Proxy @({b} {m})) i)
>             assertBool (show (n, m, i)) (maybeConvertLaw2 (Proxy @({b} {m})) i)
>             assertBool (show (n, m, i)) (maybeConvertLaw3 (Proxy @({b} {m})) i)
>             assertBool (show (n, m, i)) (maybeConvertLaw4 (Proxy @({b} {m})) i)
>  """)
-}
module Clash.Tests.MaybeNumConvert where

import Control.Monad (forM_)
import Data.Data (Proxy (..))
import Data.Either (isLeft)
import Data.Maybe (fromMaybe, isJust)
import GHC.TypeNats (someNatVal)
import Test.Tasty (TestTree, defaultMain)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Test.Tasty.TH (testGroupGenerator)

#if MIN_VERSION_base(4,18,0)
import Clash.Prelude hiding (someNatVal, withSomeSNat)
#else
import Clash.Prelude hiding (someNatVal)
#endif

#if !MIN_VERSION_base(4,16,0)
import Numeric.Natural (Natural)
#endif

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = $(testGroupGenerator)

withSomeSNat :: Natural -> (forall (n :: Nat). SNat n -> r) -> r
withSomeSNat n f = case someNatVal n of
  SomeNat (_ :: Proxy n) -> f (SNat @n)

maybeConvertLaw1 ::
  forall a b.
  (Eq a, MaybeNumConvert a b, MaybeNumConvert b a) =>
  Proxy b ->
  a ->
  Bool
maybeConvertLaw1 Proxy x =
  x == fromMaybe x (maybeNumConvert @_ @b x >>= maybeNumConvert)

maybeConvertLaw2 ::
  forall a b.
  (MaybeNumConvert a b, MaybeNumConvert b a, Integral b, Integral a) =>
  Proxy b ->
  a ->
  Bool
maybeConvertLaw2 Proxy x =
  toInteger x == fromMaybe (toInteger x) (toInteger <$> maybeNumConvert @_ @b x)

maybeConvertLaw3 ::
  forall a b.
  (MaybeNumConvert a b, MaybeNumConvert b a, Integral b, Integral a) =>
  Proxy b ->
  a ->
  Bool
maybeConvertLaw3 Proxy x =
  isJust (maybeNumConvert @_ @b x) `implies` isJust (maybeNumConvert @_ @a =<< maybeNumConvert @_ @b x)
 where
  implies :: Bool -> Bool -> Bool
  implies True False = False
  implies _ _ = True

maybeConvertLaw4 ::
  forall a b.
  (MaybeNumConvert a b, MaybeNumConvert b a, Integral b, Integral a, Bounded b, Bounded a) =>
  Proxy b ->
  a ->
  Bool
maybeConvertLaw4 Proxy x =
  isJust (maybeNumConvert @_ @b x) == (i x >= i (minBound @b) && i x <= i (maxBound @b))
 where
  i :: (Integral c) => c -> Integer
  i = toInteger

-- | Checks whether an 'XException' in, means an 'XException' out
convertXException :: forall a b. (MaybeNumConvert a b) => Proxy a -> Proxy b -> Bool
convertXException _ _ = isLeft $ isX $ maybeNumConvert @a @b (errorX "" :: a)

indexMax :: Natural
indexMax = 128

otherMax :: Natural
otherMax = 8

case_maybeConvertIndexIndex :: Assertion
case_maybeConvertIndexIndex =
  forM_ [0 .. indexMax] $ \n ->
    forM_ [0 .. indexMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) -> do
          assertBool (show (n, m)) (convertXException (Proxy @(Index (n + 1))) (Proxy @(Index (m + 1))))
          forM_ [minBound .. maxBound] $ \(i :: Index (n + 1)) -> do
            assertBool (show (n, m, i)) (maybeConvertLaw1 (Proxy @(Index (m + 1))) i)
            assertBool (show (n, m, i)) (maybeConvertLaw2 (Proxy @(Index (m + 1))) i)
            assertBool (show (n, m, i)) (maybeConvertLaw3 (Proxy @(Index (m + 1))) i)
            assertBool (show (n, m, i)) (maybeConvertLaw4 (Proxy @(Index (m + 1))) i)

case_maybeConvertIndexUnsigned :: Assertion
case_maybeConvertIndexUnsigned =
  forM_ [0 .. indexMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) -> do
          assertBool (show (n, m)) (convertXException (Proxy @(Index (n + 1))) (Proxy @(Unsigned m)))
          forM_ [minBound .. maxBound] $ \(i :: Index (n + 1)) -> do
            assertBool (show (n, m, i)) (maybeConvertLaw1 (Proxy @(Unsigned m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw2 (Proxy @(Unsigned m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw3 (Proxy @(Unsigned m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw4 (Proxy @(Unsigned m)) i)

case_maybeConvertIndexSigned :: Assertion
case_maybeConvertIndexSigned =
  forM_ [0 .. indexMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) -> do
          assertBool (show (n, m)) (convertXException (Proxy @(Index (n + 1))) (Proxy @(Signed m)))
          forM_ [minBound .. maxBound] $ \(i :: Index (n + 1)) -> do
            assertBool (show (n, m, i)) (maybeConvertLaw1 (Proxy @(Signed m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw2 (Proxy @(Signed m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw3 (Proxy @(Signed m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw4 (Proxy @(Signed m)) i)

case_maybeConvertIndexBitVector :: Assertion
case_maybeConvertIndexBitVector =
  forM_ [0 .. indexMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) -> do
          assertBool (show (n, m)) (convertXException (Proxy @(Index (n + 1))) (Proxy @(BitVector m)))
          forM_ [minBound .. maxBound] $ \(i :: Index (n + 1)) -> do
            assertBool (show (n, m, i)) (maybeConvertLaw1 (Proxy @(BitVector m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw2 (Proxy @(BitVector m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw3 (Proxy @(BitVector m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw4 (Proxy @(BitVector m)) i)

case_maybeConvertUnsignedIndex :: Assertion
case_maybeConvertUnsignedIndex =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. indexMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) -> do
          assertBool (show (n, m)) (convertXException (Proxy @(Unsigned n)) (Proxy @(Index (m + 1))))
          forM_ [minBound .. maxBound] $ \(i :: Unsigned n) -> do
            assertBool (show (n, m, i)) (maybeConvertLaw1 (Proxy @(Index (m + 1))) i)
            assertBool (show (n, m, i)) (maybeConvertLaw2 (Proxy @(Index (m + 1))) i)
            assertBool (show (n, m, i)) (maybeConvertLaw3 (Proxy @(Index (m + 1))) i)
            assertBool (show (n, m, i)) (maybeConvertLaw4 (Proxy @(Index (m + 1))) i)

case_maybeConvertUnsignedUnsigned :: Assertion
case_maybeConvertUnsignedUnsigned =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) -> do
          assertBool (show (n, m)) (convertXException (Proxy @(Unsigned n)) (Proxy @(Unsigned m)))
          forM_ [minBound .. maxBound] $ \(i :: Unsigned n) -> do
            assertBool (show (n, m, i)) (maybeConvertLaw1 (Proxy @(Unsigned m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw2 (Proxy @(Unsigned m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw3 (Proxy @(Unsigned m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw4 (Proxy @(Unsigned m)) i)

case_maybeConvertUnsignedSigned :: Assertion
case_maybeConvertUnsignedSigned =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) -> do
          assertBool (show (n, m)) (convertXException (Proxy @(Unsigned n)) (Proxy @(Signed m)))
          forM_ [minBound .. maxBound] $ \(i :: Unsigned n) -> do
            assertBool (show (n, m, i)) (maybeConvertLaw1 (Proxy @(Signed m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw2 (Proxy @(Signed m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw3 (Proxy @(Signed m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw4 (Proxy @(Signed m)) i)

case_maybeConvertUnsignedBitVector :: Assertion
case_maybeConvertUnsignedBitVector =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) -> do
          assertBool (show (n, m)) (convertXException (Proxy @(Unsigned n)) (Proxy @(BitVector m)))
          forM_ [minBound .. maxBound] $ \(i :: Unsigned n) -> do
            assertBool (show (n, m, i)) (maybeConvertLaw1 (Proxy @(BitVector m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw2 (Proxy @(BitVector m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw3 (Proxy @(BitVector m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw4 (Proxy @(BitVector m)) i)

case_maybeConvertSignedIndex :: Assertion
case_maybeConvertSignedIndex =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. indexMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) -> do
          assertBool (show (n, m)) (convertXException (Proxy @(Signed n)) (Proxy @(Index (m + 1))))
          forM_ [minBound .. maxBound] $ \(i :: Signed n) -> do
            assertBool (show (n, m, i)) (maybeConvertLaw1 (Proxy @(Index (m + 1))) i)
            assertBool (show (n, m, i)) (maybeConvertLaw2 (Proxy @(Index (m + 1))) i)
            assertBool (show (n, m, i)) (maybeConvertLaw3 (Proxy @(Index (m + 1))) i)
            assertBool (show (n, m, i)) (maybeConvertLaw4 (Proxy @(Index (m + 1))) i)

case_maybeConvertSignedUnsigned :: Assertion
case_maybeConvertSignedUnsigned =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) -> do
          assertBool (show (n, m)) (convertXException (Proxy @(Signed n)) (Proxy @(Unsigned m)))
          forM_ [minBound .. maxBound] $ \(i :: Signed n) -> do
            assertBool (show (n, m, i)) (maybeConvertLaw1 (Proxy @(Unsigned m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw2 (Proxy @(Unsigned m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw3 (Proxy @(Unsigned m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw4 (Proxy @(Unsigned m)) i)

case_maybeConvertSignedSigned :: Assertion
case_maybeConvertSignedSigned =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) -> do
          assertBool (show (n, m)) (convertXException (Proxy @(Signed n)) (Proxy @(Signed m)))
          forM_ [minBound .. maxBound] $ \(i :: Signed n) -> do
            assertBool (show (n, m, i)) (maybeConvertLaw1 (Proxy @(Signed m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw2 (Proxy @(Signed m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw3 (Proxy @(Signed m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw4 (Proxy @(Signed m)) i)

case_maybeConvertSignedBitVector :: Assertion
case_maybeConvertSignedBitVector =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) -> do
          assertBool (show (n, m)) (convertXException (Proxy @(Signed n)) (Proxy @(BitVector m)))
          forM_ [minBound .. maxBound] $ \(i :: Signed n) -> do
            assertBool (show (n, m, i)) (maybeConvertLaw1 (Proxy @(BitVector m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw2 (Proxy @(BitVector m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw3 (Proxy @(BitVector m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw4 (Proxy @(BitVector m)) i)

case_maybeConvertBitVectorIndex :: Assertion
case_maybeConvertBitVectorIndex =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. indexMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) -> do
          assertBool (show (n, m)) (convertXException (Proxy @(BitVector n)) (Proxy @(Index (m + 1))))
          forM_ [minBound .. maxBound] $ \(i :: BitVector n) -> do
            assertBool (show (n, m, i)) (maybeConvertLaw1 (Proxy @(Index (m + 1))) i)
            assertBool (show (n, m, i)) (maybeConvertLaw2 (Proxy @(Index (m + 1))) i)
            assertBool (show (n, m, i)) (maybeConvertLaw3 (Proxy @(Index (m + 1))) i)
            assertBool (show (n, m, i)) (maybeConvertLaw4 (Proxy @(Index (m + 1))) i)

case_maybeConvertBitVectorUnsigned :: Assertion
case_maybeConvertBitVectorUnsigned =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) -> do
          assertBool (show (n, m)) (convertXException (Proxy @(BitVector n)) (Proxy @(Unsigned m)))
          forM_ [minBound .. maxBound] $ \(i :: BitVector n) -> do
            assertBool (show (n, m, i)) (maybeConvertLaw1 (Proxy @(Unsigned m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw2 (Proxy @(Unsigned m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw3 (Proxy @(Unsigned m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw4 (Proxy @(Unsigned m)) i)

case_maybeConvertBitVectorSigned :: Assertion
case_maybeConvertBitVectorSigned =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) -> do
          assertBool (show (n, m)) (convertXException (Proxy @(BitVector n)) (Proxy @(Signed m)))
          forM_ [minBound .. maxBound] $ \(i :: BitVector n) -> do
            assertBool (show (n, m, i)) (maybeConvertLaw1 (Proxy @(Signed m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw2 (Proxy @(Signed m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw3 (Proxy @(Signed m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw4 (Proxy @(Signed m)) i)

case_maybeConvertBitVectorBitVector :: Assertion
case_maybeConvertBitVectorBitVector =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) -> do
          assertBool (show (n, m)) (convertXException (Proxy @(BitVector n)) (Proxy @(BitVector m)))
          forM_ [minBound .. maxBound] $ \(i :: BitVector n) -> do
            assertBool (show (n, m, i)) (maybeConvertLaw1 (Proxy @(BitVector m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw2 (Proxy @(BitVector m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw3 (Proxy @(BitVector m)) i)
            assertBool (show (n, m, i)) (maybeConvertLaw4 (Proxy @(BitVector m)) i)
