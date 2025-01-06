{-|
Copyright  :  (C) 2024-2025, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.Tests.Laws.Finite (tests) where

import Prelude hiding (reverse)

import Control.DeepSeq (NFData)
import Control.Monad (forM_)
import Data.Constraint (Dict(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product)
import Data.Functor.Sum (Sum)
import Data.Int (Int8, Int16)
#if MIN_VERSION_base(4,15,0)
import Data.Ord (Down(..))
#endif
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable, typeRep)
import Data.Void (Void)
import Data.Word (Word8, Word16)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, (@=?), testCase)

import Clash.Class.Finite (Finite(..))
import Clash.Promoted.Nat (SNatLE(..), SNat(..), compareSNat)
import Clash.Sized.BitVector (BitVector, Bit)
import Clash.Sized.Index (Index)
import Clash.Sized.RTree (RTree)
import Clash.Sized.Signed (Signed)
import Clash.Sized.Unsigned (Unsigned)
import Clash.Sized.Vector (Vec, indicesI, iterateI, reverse)

indexOrderLaw ::
  forall a.
  (NFData a, Show a, Finite a) =>
  Proxy a ->
  Assertion
indexOrderLaw Proxy =
  index <$> elements @a @=? indicesI

forwardIterateLaw ::
  forall a.
  (NFData a, Show a, Eq a, Finite a) =>
  Proxy a ->
  Assertion
forwardIterateLaw Proxy =
  iterateI (>>= succMaybe) (lowestMaybe @a) @=? Just <$> elements @a

backwardIterateLaw ::
  forall a.
  (NFData a, Show a, Eq a, Finite a) =>
  Proxy a ->
  Assertion
backwardIterateLaw Proxy =
  iterateI (>>= predMaybe) (highestMaybe @a) @=? Just <$> reverse (elements @a)

indexIsomorphismLaw ::
  forall a.
  (NFData a, Show a, Eq a, Finite a) =>
  Proxy a ->
  Assertion
indexIsomorphismLaw Proxy =
  ith . index <$> elements @a @=? elements @a

minimumPredecessor ::
  forall a.
  (NFData a, Show a, Eq a, Finite a) =>
  Proxy a ->
  Assertion
minimumPredecessor Proxy =
  (lowestMaybe >>= predMaybe @a) @=? Nothing

maximumSuccessor ::
  forall a.
  (NFData a, Show a, Eq a, Finite a) =>
  Proxy a ->
  Assertion
maximumSuccessor Proxy =
  (highestMaybe >>= succMaybe @a) @=? Nothing

extremes ::
  forall a.
  (NFData a, Show a, Eq a, Finite a) =>
  Proxy a ->
  Assertion
extremes Proxy = case compareSNat (SNat @1) (SNat @(ElementCount a)) of
  SNatLE -> do
    lowestMaybe @a @=? Just lowest
    highestMaybe @a @=? Just highest
  SNatGT -> do
    lowestMaybe @a @=? Nothing
    highestMaybe @a @=? Nothing

boundedCompatibility ::
  forall a.
  (NFData a, Show a, Eq a, Finite a) =>
  Maybe (Dict (Bounded a)) ->
  Assertion
boundedCompatibility = \case
  Nothing -> return ()
  Just Dict -> case compareSNat (SNat @1) (SNat @(ElementCount a)) of
    SNatGT -> return ()
    SNatLE -> do
      lowest @a @=? minBound @a
      highest @a @=? maxBound @a

enumCompatibility ::
  forall a.
  (NFData a, Show a, Eq a, Finite a) =>
  Maybe (Dict (Enum a)) ->
  Assertion
enumCompatibility = \case
  Nothing -> return ()
  Just Dict -> forM_ (elements @a) $ \x -> do
    maybe (return ()) (@=? succ x) $ succMaybe x
    maybe (return ()) (@=? pred x) $ predMaybe x

finiteLaws ::
  forall a.
  (NFData a, Show a, Eq a, Finite a) =>
  Maybe (Dict (Bounded a)) ->
  Maybe (Dict (Enum a)) ->
  [TestTree]
finiteLaws mBounded mEnum =
  [ testCase "Index Order"           $ indexOrderLaw proxy
  , testCase "Forward Iterate"       $ forwardIterateLaw proxy
  , testCase "Backward Iterate"      $ backwardIterateLaw proxy
  , testCase "Index Isomorphism"     $ indexIsomorphismLaw proxy
  , testCase "Minimum Predecessor"   $ minimumPredecessor proxy
  , testCase "Maximum Successor"     $ maximumSuccessor proxy
  , testCase "Extremes"              $ extremes proxy
  , testCase "Bounded Compatibility" $ boundedCompatibility mBounded
  , testCase "Enum Compatibility"    $ enumCompatibility mEnum
  ]
 where
  proxy :: Proxy a
  proxy = Proxy

testFiniteLaws ::
  forall a.
  (NFData a, Show a, Eq a, Finite a, Typeable a) =>
  Maybe (Dict (Bounded a)) ->
  Maybe (Dict (Enum a)) ->
  TestTree
testFiniteLaws mBounded mEnum =
  testGroup (show (typeRep proxy)) $ finiteLaws mBounded mEnum
 where
  proxy :: Proxy a
  proxy = Proxy

tests :: TestTree
tests = testGroup "Finite"
  [ testFiniteLaws noBInst  $ noEInst  @Void
  , testFiniteLaws hasBInst $ hasEInst @()
  , testFiniteLaws hasBInst $ hasEInst @Bit
  , testFiniteLaws hasBInst $ hasEInst @Bool
  , testFiniteLaws hasBInst $ hasEInst @Ordering

  , testFiniteLaws hasBInst $ hasEInst @Char
  , testFiniteLaws hasBInst $ hasEInst @Int8
  , testFiniteLaws hasBInst $ hasEInst @Int16
  , testFiniteLaws hasBInst $ hasEInst @Word8
  , testFiniteLaws hasBInst $ hasEInst @Word16

  , testFiniteLaws hasBInst $ hasEInst @(BitVector 0)
  , testFiniteLaws hasBInst $ hasEInst @(BitVector 1)
  , testFiniteLaws hasBInst $ hasEInst @(BitVector 8)

  , testFiniteLaws hasBInst $ hasEInst @(Index 0)
  , testFiniteLaws hasBInst $ hasEInst @(Index 1)
  , testFiniteLaws hasBInst $ hasEInst @(Index 128)

  , testFiniteLaws hasBInst $ hasEInst @(Signed 0)
  , testFiniteLaws hasBInst $ hasEInst @(Signed 1)
  , testFiniteLaws hasBInst $ hasEInst @(Signed 8)

  , testFiniteLaws hasBInst $ hasEInst @(Unsigned 0)
  , testFiniteLaws hasBInst $ hasEInst @(Unsigned 1)
  , testFiniteLaws hasBInst $ hasEInst @(Unsigned 8)

  , testFiniteLaws noBInst  $ noEInst  @(Maybe (Index 0))
  , testFiniteLaws noBInst  $ noEInst  @(Maybe (Index 1))
  , testFiniteLaws noBInst  $ noEInst  @(Maybe (Index 27))

  , testFiniteLaws noBInst  $ noEInst  @(Either Void (Index 0))
  , testFiniteLaws noBInst  $ noEInst  @(Either Void (Index 1))
  , testFiniteLaws noBInst  $ noEInst  @(Either Void (Index 27))
  , testFiniteLaws noBInst  $ noEInst  @(Either Bool (Index 0))
  , testFiniteLaws noBInst  $ noEInst  @(Either Bool (Index 1))
  , testFiniteLaws noBInst  $ noEInst  @(Either Bool (Index 27))

  , testFiniteLaws noBInst  $ noEInst  @(Compose Maybe Maybe Bool)
  , testFiniteLaws hasBInst $ hasEInst @(Const Bool [Int])
#if MIN_VERSION_base(4,15,0)
  , testFiniteLaws hasBInst $ hasEInst @(Down Bool)
#endif
  , testFiniteLaws hasBInst $ hasEInst @(Identity Bool)
  , testFiniteLaws noBInst  $ noEInst  @(Product Maybe Maybe Bit)
  , testFiniteLaws noBInst  $ noEInst  @(Sum Maybe Maybe Bit)

  , testFiniteLaws noBInst  $ noEInst  @(Vec 0 Void)
  , testFiniteLaws noBInst  $ noEInst  @(Vec 1 Void)
  , testFiniteLaws noBInst  $ noEInst  @(Vec 16 Void)
  , testFiniteLaws noBInst  $ noEInst  @(Vec 0 Bool)
  , testFiniteLaws noBInst  $ noEInst  @(Vec 1 Bool)
  , testFiniteLaws noBInst  $ noEInst  @(Vec 16 Bool)

  , testFiniteLaws noBInst  $ noEInst  @(RTree 0 Void)
  , testFiniteLaws noBInst  $ noEInst  @(RTree 1 Void)
  , testFiniteLaws noBInst  $ noEInst  @(RTree 4 Void)
  , testFiniteLaws noBInst  $ noEInst  @(RTree 0 Bool)
  , testFiniteLaws noBInst  $ noEInst  @(RTree 1 Bool)
  , testFiniteLaws noBInst  $ noEInst  @(RTree 4 Bool)

  , testFiniteLaws noBInst  $ noEInst  @(Void, Void)
  , testFiniteLaws noBInst  $ noEInst  @(Bool, Void)
  , testFiniteLaws noBInst  $ noEInst  @(Void, Bool)
  , testFiniteLaws noBInst  $ noEInst  @(Bool, Bool)

  , testFiniteLaws noBInst  $ noEInst  @(Bool, Bool, Bool)
  , testFiniteLaws noBInst  $ noEInst  @(Void, Bool, Bool)
  , testFiniteLaws noBInst  $ noEInst  @(Bool, Void, Bool)
  , testFiniteLaws noBInst  $ noEInst  @(Bool, Bool, Void)

  , testFiniteLaws noBInst  $ noEInst  @(Bool, Bool, Bool, Bool)
  ]
 where
  noBInst :: Maybe (Dict (Bounded a))
  noBInst = Nothing

  hasBInst :: Bounded a => Maybe (Dict (Bounded a))
  hasBInst = Just Dict

  noEInst :: Maybe (Dict (Enum a))
  noEInst = Nothing

  hasEInst :: Enum a => Maybe (Dict (Enum a))
  hasEInst = Just Dict
