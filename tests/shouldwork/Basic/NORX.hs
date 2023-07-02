{-# LANGUAGE CPP #-}

module NORX where
import Data.Bits
import Clash.Prelude
import Clash.Explicit.Testbench

type W = Unsigned 32

norx :: Vec 16 W -> Vec 16 W
norx inp = iterate d5 f inp !! 4

f :: Vec 16 W -> Vec 16 W
f inp = result
  where
    (s0:>s1:>s2:>s3:>s4:>s5:>s6:>s7:>s8:>s9:>s10:>s11:>s12:>s13:>s14:>s15:>_) = inp

    -- Column round
    (t0, t4, t8,  t12) = g (s0, s4, s8,  s12)
    (t1, t5, t9,  t13) = g (s1, s5, s9,  s13)
    (t2, t6, t10, t14) = g (s2, s6, s10, s14)
    (t3, t7, t11, t15) = g (s3, s7, s11, s15)
    -- Diagonal round
    (u0, u5, u10, u15) = g (t0, t5, t10, t15)
    (u1, u6, u11, u12) = g (t1, t6, t11, t12)
    (u2, u7, u8,  u13) = g (t2, t7, t8,  t13)
    (u3, u4, u9,  u14) = g (t3, t4, t9,  t14)

    result = (u0:>u1:>u2:>u3:>u4:>u5:>u6:>u7:>u8:>u9:>u10:>u11:>u12:>u13:>u14:>u15:>Nil)

g :: (W, W, W, W) -> (W, W, W, W)
g (a, b, c, d) = (a'', b'', c'', d'')
  where
    a'  = h a b
    d'  = (a' `xor` d)   `rotateR` 8
    c'  = h c d'
    b'  = (b `xor` c')   `rotateR` 11
    a'' = h a' b'
    d'' = (a'' `xor` d') `rotateR` 16
    c'' = h c' d''
    b'' = (b' `xor` c'') `rotateR` 31

h :: W -> W -> W
h x y = (x `xor` y) `xor` ((x .&. y) `shiftL` 1)

--------------------------------------------------------------------------------
-- HDL export interface

topEntity :: Vec 16 W -> Vec 16 W
topEntity = norx
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst ((0:>1:>2:>3:>4:>5:>6:>7:>8:>9:>10:>11:>12:>13:>14:>15:>Nil):>Nil)
    expectedOutput = outputVerifier'   clk rst
                                      ((   0x99a0283a
                                        :> 0x16c4b42e
                                        :> 0x6e7fa00b
                                        :> 0x7d075c66
                                        :> 0x65c1af81
                                        :> 0xee254c00
                                        :> 0x126631b6
                                        :> 0xf8915260
                                        :> 0x083181d5
                                        :> 0x85dc0152
                                        :> 0x1a44a1f3
                                        :> 0x7ba61b1a
                                        :> 0x37dde5df
                                        :> 0x078203d3
                                        :> 0x9b3c0701
                                        :> 0x9ce6be37
                                        :> Nil) :> Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
