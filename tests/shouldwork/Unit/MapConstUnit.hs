{-# LANGUAGE CPP #-}

module MapConstUnit where

import Clash.Prelude
import Clash.Explicit.Testbench

zeroF
  :: KnownNat n
  => (BitVector n -> BitVector n -> (BitVector n, BitVector n))
  -> Vec m (BitVector n)
  -> Vec m (BitVector n)
  -> Vec m (BitVector 8)
zeroF f a b =
  zipWith (zeroExtend . uncurry (++#) . f) a b

topEntity :: Signal System (Vec 5 Int)
topEntity = pure (snd (mapAccumL (\acc _ -> (succ acc, acc)) 0 (map (const ()) indicesI)))
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier' clk rst ((0 :> 1 :> 2 :> 3 :> 4 :> Nil) :> Nil)

    done           = expectedOutput topEntity
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
