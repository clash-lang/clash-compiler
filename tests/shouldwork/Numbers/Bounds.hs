{-# LANGUAGE CPP #-}

module Bounds where
import Clash.Prelude
import Clash.Explicit.Testbench
import Data.Int

expected
    =     0 :>  42
    :> -256 :> 255
    :>    0 :> 511
    :>    0 :> 511
    :>  -64 :>  63
    :>    0 :> 127
    :> (Nil::Vec 0 Int64)

actual
    =  numConvert (minBound :: Index 43)             :> numConvert (maxBound :: Index 43)
    :> numConvert (minBound :: Signed 9)             :> numConvert (maxBound :: Signed 9)
    :> numConvert (minBound :: Unsigned 9)           :> numConvert (maxBound :: Unsigned 9)
    :> numConvert (minBound :: BitVector 9)          :> numConvert (maxBound :: BitVector 9)
    :> numConvert (unFixed (minBound :: SFixed 4 3)) :> numConvert (unFixed (maxBound :: SFixed 4 3))
    :> numConvert (unFixed (minBound :: UFixed 3 4)) :> numConvert (unFixed (maxBound :: UFixed 3 4))
    :> Nil

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System () -> Signal System Int64
topEntity = exposeClockResetEnable (mealy loop actual)
{-# OPAQUE topEntity #-}

loop :: Vec (n+2) a -> () -> (Vec (n+2) a, a)
--loop (x:>xs) _ = (xs :< last xs, x)
loop xs _ = (xs <<+ last xs, head xs)

testBench :: Signal System Bool
testBench = done
  where
    testInput      = pure ()
    expectedOutput = outputVerifier' clk rst expected
    done           = expectedOutput (topEntity clk rst enableGen testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
