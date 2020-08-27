{-# LANGUAGE TypeFamilies #-}

module Clash.Tests.Resize (tests) where

import Control.DeepSeq (NFData)
import Control.Exception (SomeException, try, evaluate)
import Clash.XException (XException)
import Data.Either (isLeft)
import Data.Proxy (Proxy(Proxy))
import GHC.TypeNats (KnownNat, type (<=))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import qualified Clash.Class.Resize as Resize
import Clash.Sized.Index

-- | Anything that's in bounds should not cause an error
indexProp ::
  forall a b.
  ((a <= b), KnownNat a, KnownNat b) =>
  Proxy b -> Index a -> Bool
indexProp Proxy v =
  Resize.resize v == Resize.checkedResize @a @b v

-- | Anything that's out of bounds should cause an error
indexFailProp ::
  forall a b.
  ((b <= a), KnownNat a, KnownNat b) =>
  Proxy b -> Index a -> Property
indexFailProp Proxy v =
  let checked = Resize.checkedResize @a @b v in
  if toInteger v > toInteger (maxBound @(Index b)) then
    expectExceptionNoX checked
  else
    discard

-- | Succeed if evaluating leads to a non-XException Exception
expectExceptionNoX :: (Show a, NFData a) => a -> Property
expectExceptionNoX a0 = ioProperty $ do
  a1 <- try @SomeException (try @XException (evaluate a0))
  pure $
    counterexample
      ("Expected non-XException Exception, got: " <> show a1)
      (isLeft a1)

tests :: TestTree
tests = testGroup "Resize"
  [ testGroup "checkedResize"
    [ testProperty "indexProp @17 @19" (indexProp @17 @19 Proxy)
    , testProperty "indexProp @19 @19" (indexProp @19 @19 Proxy)
    , testProperty "indexFailProp @37 @7" (indexFailProp @37 @7 Proxy)
    ]
  ]
