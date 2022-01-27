{-# LANGUAGE NoImplicitPrelude #-}

module Clash.Tests.BlockRam (tests) where

import qualified Data.List as List
import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Explicit.Prelude as E
import Clash.Explicit.BlockRam (blockRam#)
import Clash.Prelude

readRam
  :: (HiddenClockResetEnable dom)
  => Signal dom (Unsigned 4)
  -> Signal dom (Unsigned 8)
readRam addr = mux (register False $ addr .<. 8) ram (pure 0xff)
  where
    ram = blockRam1 NoClearOnReset (SNat @8) 0 addr (pure Nothing)

-- If the block RAM uses the address argument too strictly, then it will
-- attempt to access an out of bounds address when using readRam.
--
addrNotTooStrict :: Assertion
addrNotTooStrict =
  let addr = fromList [0..15]
   in List.tail (sampleN @System 15 (readRam addr)) @?=
        [255,0,0,0,0,0,0,0,255,255,255,255,255,255]

primRam
  :: Signal System Int
  -> Signal System Bool
  -> Signal System Int
  -> Signal System Int
  -> Signal System (Maybe Int)
primRam rd we wr din =
  maybeIsX <$> blockRam# clockGen enableGen (0 :> 1 :> Nil) rd we wr din

type PrimSamples = [(Int, Bool, Int, Int, Maybe Int)]

undefEn :: PrimSamples

-- Undefined enable:
--    The written-to address should read 'undefined', but other addresses
--    should still have their data.

--                               rd  enable     waddr      wdata      dout

undefEn =                     [ ( 0, undefined, 0        , 2        , Nothing)
                              , ( 0, False    , 0        , 3        , Just 0 )
                              , ( 1, False    , 0        , 3        , Nothing)
                              , ( 1, False    , 0        , 3        , Just 1 )
                              ]

primRamAssertion
  :: PrimSamples
  -> Assertion
primRamAssertion samples = actual @?= expectedOutput
 where
  (rd, we, wr, din, expectedOutput) = List.unzip5 samples
  actual = E.sampleN (List.length samples) $ primRam (fromList rd)
                                                     (fromList we)
                                                     (fromList wr)
                                                     (fromList din)

tests :: TestTree
tests = testGroup "BlockRam"
  [ testCase "Address strictness" addrNotTooStrict
  , testCase "Undefined enable" $ primRamAssertion undefEn
  ]
