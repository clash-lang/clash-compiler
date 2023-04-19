{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Cores.Xilinx.BlockRam where

import Clash.Explicit.Prelude
import Clash.Sized.Internal.BitVector (BitVector(..))
import Clash.XException.MaybeX (MaybeX)

import Control.DeepSeq (force)
import Data.Bifunctor (bimap)
import Data.Sequence (Seq)
import Data.Word (Word16)
import Hedgehog
import Numeric (showHex)
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit

import Clash.Hedgehog.Sized.BitVector (genBitVector)

import Clash.Cores.Xilinx.BlockRam.Internal
  (mergeEnableByteEnable, isActiveWriteEnable, updateRam)

import qualified Clash.Explicit.BlockRam.Model as BlockRamModel

import qualified Prelude as P
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

newtype ShowAsHex a = ShowAsHex a

instance (Integral a, Show a) => Show (ShowAsHex a) where
  showsPrec _ (ShowAsHex x) = ("0x" <>) . pad (showHex x "")
   where
    pad hex = (P.replicate (4 - P.length hex) '0' <>) . (hex <>)

instance (Integral a, Show a) => ShowX (ShowAsHex a) where
  showsPrecX = showsPrecXWith showsPrec

-- | 'BlockRamModel.accessRam', but specialized for byte enables.
accessRam ::
  ( KnownNat nBytes
  , BitSize a ~ (8 * nBytes)
  , NFDataX a
  , BitPack a
  ) =>
  SNat nAddrs ->
  MaybeX Int ->
  MaybeX (BitVector nBytes) ->
  a ->
  Seq.Seq a ->
  (a, Seq.Seq a)
accessRam snat =
  BlockRamModel.accessRam
    snat
    isActiveWriteEnable
    updateRam

-- | 'BlockRamModel.cycleBoth', but specialized for byte enables.
cycleBoth ::
  ( KnownNat nBytes
  , BitSize a ~ (8 * nBytes)
  , NFDataX a
  , BitPack a
  ) =>
  SNat nAddrs ->
  a ->
  a ->
  Seq a ->
  (MaybeX Bool, MaybeX Int, MaybeX (BitVector nBytes), a) ->
  (MaybeX Bool, MaybeX Int, MaybeX (BitVector nBytes), a) ->
  (Seq a, a, a)
cycleBoth snat =
  BlockRamModel.cycleBoth
    snat
    (BlockRamModel.TdpbramModelConfig isActiveWriteEnable mergeEnableByteEnable updateRam)

-- | Generate a value with given generator OR generate an 'XException'.
genOrX :: String -> Gen a -> Gen a
genOrX msg genA = Gen.choice [genA, Gen.constant (errorX msg)]

-- | No matter the initial state nor the output, the output of 'accessRam'
-- should look like the following:
--
--   * The new RAM contents should be spine defined
--   * All elements of the RAM should be defined or X
--   * The outputs should be defined or X
--
prop_definedOutputAccessRam ::
  ( KnownNat nBytes
  , BitSize a ~ (8 * nBytes)
  , NFDataX a
  , BitPack a
  , Show a
  ) =>
  Gen a ->
  Property
prop_definedOutputAccessRam genA = property $ do
  initMem <- forAll $ Seq.fromList <$> genInitMem
  addr    <- forAll $ genOrX "addr" genAddr
  byteEna <- forAll $ genOrX "byteEna" genBitVector
  dat     <- forAll $ genOrX "dat" genA

  let (result, mem) = access initMem addr byteEna dat

  -- Result should be defined or X
  !_ <- pure (isX (forceX result))

  -- Memory should be defined or X
  !_ <- pure (isX (forceX mem))

  -- Memory spine should be defined
  !_ <- pure (force (const () <$> mem))

  pure ()
 where
  sMemSize = d3
  memSize = snatToNum sMemSize

  genAddr = Gen.int (Range.linear 0 (memSize - 1))
  genInitMem = Gen.list (Range.singleton memSize) (genOrX "init" genA)

  access initMem addr byteEna dat =
    accessRam sMemSize (pure addr) (pure byteEna) dat initMem

-- | No matter the initial state nor the output, the output of 'cycleBoth' should
-- look like the following:
--
--   * The new RAM contents should be spine defined
--   * All elements of the RAM should be defined or X
--   * The outputs should be defined or X
--
prop_definedOutputCycleBoth ::
  ( KnownNat nBytes
  , BitSize a ~ (8 * nBytes)
  , NFDataX a
  , BitPack a
  , Show a
  ) =>
  Gen a ->
  Property
prop_definedOutputCycleBoth genA = property $ do
  initMem <- forAll $ Seq.fromList <$> genInitMem

  -- Port A
  enaA     <- forAll $ genOrX "enaA" Gen.bool
  addrA    <- forAll $ genOrX "addrA" genAddr
  byteEnaA <- forAll $ genOrX "byteEnaA" genBitVector
  datA     <- forAll $ genOrX "datA" genA
  prevA    <- forAll $ genOrX "prevA" genA

  -- Port B
  enaB     <- forAll $ genOrX "enaB" Gen.bool
  addrB    <- forAll $ genOrX "addrB" genAddr
  byteEnaB <- forAll $ genOrX "byteEnaB" genBitVector
  datB     <- forAll $ genOrX "datB" genA
  prevB    <- forAll $ genOrX "prevB" genA

  let
    (mem, a, b) = cycleBoth sMemSize prevA prevB initMem portA portB
    portA = (pure enaA, pure addrA, pure byteEnaA, datA)
    portB = (pure enaB, pure addrB, pure byteEnaB, datB)

  -- Result should be defined or X
  !_ <- pure (isX (forceX a))
  !_ <- pure (isX (forceX b))

  -- Memory should be defined or X
  !_ <- pure (isX (forceX mem))

  -- Memory spine should be defined
  !_ <- pure (force (const () <$> mem))

  pure ()
 where
  sMemSize = d3
  memSize = snatToNum sMemSize

  genAddr = Gen.int (Range.linear 0 (memSize - 1))
  genInitMem = Gen.list (Range.singleton memSize) (genOrX "init" genA)

testsCycleBoth :: TestTree
testsCycleBoth = testGroup "cycleBoth"
  [ testCase "test1"  $ cyc doNothing           doNothing          @?= "([11,22],33,44)"
  , testCase "test2"  $ cyc (readFromAddr 0)    doNothing          @?= "([11,22],11,44)"
  , testCase "test3"  $ cyc doNothing           (readFromAddr 0)   @?= "([11,22],33,11)"
  , testCase "test4"  $ cyc (readFromAddr 0)    (readFromAddr 0)   @?= "([11,22],11,11)"
  , testCase "test5"  $ cyc (writeToAddr 0 66)   doNothing         @?= "([66,22],66,44)"
  , testCase "test6"  $ cyc doNothing           (writeToAddr 0 66) @?= "([66,22],33,66)"
  , testCase "test7"  $ cyc (writeToAddr 0 66)  (writeToAddr 1 77) @?= "([66,77],66,77)"
  , testCase "test8"  $ cyc (writeToAddr 0 66)  (writeToAddr 0 77) @?= "([undefined,22],undefined,undefined)"
  , testCase "test9"  $ cyc (writeToAddr 0 66)  (writeToAddr 0 66) @?= "([undefined,22],undefined,undefined)"

    -- Write with undefined enable
  , testCase "test10" $ cyc (uReadFromAddr 0)   doNothing           @?= "([11,22],undefined,44)"
  , testCase "test11" $ cyc doNothing           (uReadFromAddr 0)   @?= "([11,22],33,undefined)"
  , testCase "test12" $ cyc (uReadFromAddr 0)   (uReadFromAddr 0)   @?= "([11,22],undefined,undefined)"
  , testCase "test13" $ cyc (uWriteToAddr 0 66) doNothing           @?= "([undefined,22],undefined,44)"
  , testCase "test14" $ cyc doNothing           (uWriteToAddr 0 66) @?= "([undefined,22],33,undefined)"
  , testCase "test15" $ cyc (uWriteToAddr 0 66) (uWriteToAddr 1 77) @?= "([undefined,undefined],undefined,undefined)"
  , testCase "test16" $ cyc (uWriteToAddr 0 66) (uWriteToAddr 0 77) @?= "([undefined,22],undefined,undefined)"
  , testCase "test17" $ cyc (uWriteToAddr 0 66) (uWriteToAddr 0 66) @?= "([undefined,22],undefined,undefined)"

  -- Repeat earlier tests, but with undefined enable
  , testCase "test18"  $ cyc (readFromAddr u)    doNothing           @?= "([11,22],undefined,44)"
  , testCase "test19"  $ cyc doNothing           (readFromAddr u)    @?= "([11,22],33,undefined)"
  , testCase "test20"  $ cyc (readFromAddr u)    (readFromAddr u)    @?= "([11,22],undefined,undefined)"
  , testCase "test21"  $ cyc (writeToAddr u 66)   doNothing          @?= "([undefined,undefined],undefined,44)"
  , testCase "test22"  $ cyc doNothing           (writeToAddr u 66)  @?= "([undefined,undefined],33,undefined)"
  , testCase "test23"  $ cyc (writeToAddr u 66)  (writeToAddr u 77)  @?= "([undefined,undefined],undefined,undefined)"
  , testCase "test24"  $ cyc (writeToAddr u 66)  (writeToAddr u 77)  @?= "([undefined,undefined],undefined,undefined)"
  , testCase "test25"  $ cyc (writeToAddr u 66)  (writeToAddr u 66)  @?= "([undefined,undefined],undefined,undefined)"
  , testCase "test26"  $ cyc (uReadFromAddr u)   doNothing           @?= "([11,22],undefined,44)"
  , testCase "test27"  $ cyc doNothing           (uReadFromAddr u)   @?= "([11,22],33,undefined)"
  , testCase "test28"  $ cyc (uReadFromAddr u)   (uReadFromAddr u)   @?= "([11,22],undefined,undefined)"
  , testCase "test29"  $ cyc (uWriteToAddr u 66) doNothing           @?= "([undefined,undefined],undefined,44)"
  , testCase "test30"  $ cyc doNothing           (uWriteToAddr u 66) @?= "([undefined,undefined],33,undefined)"
  , testCase "test31"  $ cyc (uWriteToAddr u 66) (uWriteToAddr u 77) @?= "([undefined,undefined],undefined,undefined)"
  , testCase "test32"  $ cyc (uWriteToAddr u 66) (uWriteToAddr u 77) @?= "([undefined,undefined],undefined,undefined)"
  , testCase "test33"  $ cyc (uWriteToAddr u 66) (uWriteToAddr u 66) @?= "([undefined,undefined],undefined,undefined)"

  , testProperty "prop_definedOutputCycleBothWord16"
      (prop_definedOutputCycleBoth (Gen.enumBounded @_ @Word16))
  , testProperty "prop_definedOutputCycleBothBitVector16"
      (prop_definedOutputCycleBoth (genBitVector @_ @16))
  ]
 where
  cyc portA portB = showX $
    cycleBoth d2 33 44 [11, 22 :: Word16] (portToX portA) (portToX portB)

  portToX (en, addr, byteEna, dat) = (pure en, pure addr, pure byteEna, dat)

  doNothing = (False, 0, 0, 0)
  readFromAddr addr = (True, addr, minBound :: BitVector 2, 0)
  writeToAddr addr n = (True, addr, maxBound :: BitVector 2, n)

  u = errorX "X"
  uReadFromAddr addr = (u, addr, minBound :: BitVector 2, 0)
  uWriteToAddr addr n = (u, addr, maxBound :: BitVector 2, n)

testsAccessRam :: TestTree
testsAccessRam = testGroup "accessRam"
  [ testCase "test1" $ access 0 doWrite 0x3333 @?= "(0x3333,[0x3333,0x2222])"
  , testCase "test2" $ access 0 noWrite 0x3333 @?= "(0x1111,[0x1111,0x2222])"
  , testCase "test3" $ access 0 doWrite u      @?= "(undefined,[undefined,0x2222])"

  -- Byte enables on defined data
  , testCase "test4" $ access 0 0b11 0x3333 @?= "(0x3333,[0x3333,0x2222])"
  , testCase "test5" $ access 0 0b01 0x3333 @?= "(0x1133,[0x1133,0x2222])"
  , testCase "test6" $ access 0 0b10 0x3333 @?= "(0x3311,[0x3311,0x2222])"
  , testCase "test7" $ access 0 0b00 0x3333 @?= "(0x1111,[0x1111,0x2222])"

  -- Byte enables on undefined data
  , testCase "test8"  $ access 0 0b11 u @?= "(undefined,[undefined,0x2222])"
  , testCase "test9"  $ access 0 0b01 u @?= "(undefined,[undefined,0x2222])"
  , testCase "test10" $ access 0 0b10 u @?= "(undefined,[undefined,0x2222])"
  , testCase "test11" $ access 0 0b00 u @?= "(0x1111,[0x1111,0x2222])"

  -- Repeat earlier test, but with undefined address. Note that writing with an
  -- undefined address replaces the whole memory with Xs even though partial byte
  -- enables could behave more precisely.
  , testCase "test12" $ access u 0b11 0x3333 @?= "(undefined,[undefined,undefined])"
  , testCase "test13" $ access u 0b01 0x3333 @?= "(undefined,[undefined,undefined])"
  , testCase "test14" $ access u 0b10 0x3333 @?= "(undefined,[undefined,undefined])"
  , testCase "test15" $ access u 0b00 0x3333 @?= "(undefined,[0x1111,0x2222])"
  , testCase "test16" $ access u 0b11 u      @?= "(undefined,[undefined,undefined])"
  , testCase "test17" $ access u 0b01 u      @?= "(undefined,[undefined,undefined])"
  , testCase "test18" $ access u 0b10 u      @?= "(undefined,[undefined,undefined])"
  , testCase "test19" $ access u 0b00 u      @?= "(undefined,[0x1111,0x2222])"
  , testCase "test20" $ access u u    u      @?= "(undefined,[undefined,undefined])"

  -- Byte enables on BitVector
  , testCase "test21" $ accessBv $(bLit "11") bvDat @?= "[0b...._1010_...._1010]"
  , testCase "test22" $ accessBv $(bLit "1.") bvDat @?= "[0b...._1010_...._..10]"
  , testCase "test23" $ accessBv $(bLit ".1") bvDat @?= "[0b...._...._...._1010]"
  , testCase "test24" $ accessBv $(bLit "..") bvDat @?= "[0b...._...._...._..10]"
  , testCase "test25" $ accessBv (errorX "X") bvDat @?= "[0b...._...._...._..10]"
  , testCase "test26" $ accessBv $(bLit "01") bvDat @?= "[0b...._...._...._1010]"
  , testCase "test27" $ accessBv $(bLit "10") bvDat @?= "[0b...._1010_0110_0110]"

  , testProperty "prop_definedOutputAccessRamWord16"
      (prop_definedOutputAccessRam (Gen.enumBounded @_ @Word16))
  , testProperty "prop_definedOutputAccessRamBitVector16"
      (prop_definedOutputAccessRam (genBitVector @_ @16))
  ]
 where
  access addr byteEna dat =
    showX $ bimap ShowAsHex (F.toList . fmap ShowAsHex) $
      accessRam d2 (pure addr) (pure byteEna) dat [0x1111, 0x2222 :: Word16]

  accessBv byteEna dat = showX $ F.toList $ snd $
    accessRam d1 (pure 0) (pure byteEna) dat [bvInit]

  -- bvInit and bvDat chosen to exercise all possible bit combinations of mask
  -- and value of old and new data. For the byte enables, this would be
  -- superfluous, as the @BV 1 1@ type pattern will not propagate past
  -- @byteMaskToBitMask@.
  bvInit :: BitVector 16
  bvInit =
    BV
      0b1111_1111_0000_0000
      0b0110_0110_0110_0110

  bvDat =
    BV
      0b1111_0000_1111_0000
      0b1010_1010_1010_1010

  u = errorX "X"

  doWrite :: BitVector 2
  doWrite = maxBound

  noWrite :: BitVector 2
  noWrite = 0

tests :: TestTree
tests = testGroup "Tests.Cores.Xilinx.BlockRam.Internal"
  [ testsAccessRam
  , testsCycleBoth
  ]

main :: IO ()
main = defaultMain tests
