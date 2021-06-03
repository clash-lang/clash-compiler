module Clash.Tests.BlockRam (tests) where

import qualified Data.List as List
import Test.Tasty
import Test.Tasty.HUnit

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

tests :: TestTree
tests = testGroup "BlockRam"
  [ testCase "Address strictness" addrNotTooStrict
  ]
