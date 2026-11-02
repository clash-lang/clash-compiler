module T3495_iterateI_arg_type where

import Clash.Prelude
import Clash.Explicit.Testbench

type E = Vec 2 (Unsigned 8)

-- The blackbox for 'iterateI' used to pass the wrong type to the function it
-- iterates. That crashed Clash whenever that function is itself a primitive
-- whose template inspects its argument or result type, like 'map f' or
-- 'resize'.
topEntity ::
  (E, Unsigned 8, (E, Bool)) ->
  (Vec 3 E, Vec 3 (Unsigned 8), Vec 3 (E, Bool))
topEntity (v, u, p) =
  ( iterateI (map (+1)) v
  , iterateI (resize @Unsigned @8 @8) u
  , iterateI (\(a, b) -> (map (+1) a, not b)) p
  )
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  inputs =
    (     (1 :> 2 :> Nil, 3, (5 :> 6 :> Nil, False))
       :> (254 :> 100 :> Nil, 255, (255 :> 0 :> Nil, True))
       :> (0 :> 255 :> Nil, 0, (7 :> 8 :> Nil, False))
       :> Nil )
  expected = $(lift (map
    (\(v, u, p) ->
      ( iterate d3 (map (+1)) v
      , iterate d3 (resize @Unsigned @8 @8) u
      , iterate d3 (\(a, b) -> (map (+1) a, not b)) p ))
    (     (1 :> 2 :> Nil, 3, (5 :> 6 :> Nil, False))
       :> (254 :> 100 :> Nil, 255, (255 :> 0 :> Nil, True))
       :> (0 :> 255 :> Nil, 0, (7 :> 8 :> Nil, False))
       :> Nil
       :: Vec 3 (Vec 2 (Unsigned 8), Unsigned 8, (Vec 2 (Unsigned 8), Bool)) )))

  testInput      = stimuliGenerator clk rst inputs
  expectedOutput = outputVerifier' clk rst expected
  done           = expectedOutput (topEntity <$> testInput)
  clk            = tbSystemClockGen (not <$> done)
  rst            = systemResetGen
