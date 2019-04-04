{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE UndecidableInstances #-}

module GenericBitPack where

import GenericBitPackTypes
import Clash.Prelude
import Clash.Explicit.Testbench

topEntity
  :: ( FooProduct (Unsigned 2) (Unsigned 2)
     , FooSum
     , FooSum
     , FooSum
     , FooSum
     , FooSP1 (Unsigned 3) (Unsigned 5)
     , FooSP1 (Unsigned 3) (Unsigned 5)
     , FooSP2 (Unsigned 3) (Unsigned 5)
     , FooSP2 (Unsigned 3) (Unsigned 5)
     , FooSP2 (Unsigned 3) (Unsigned 5)
     )
  -> BitVector 64
topEntity (a, b, c, d, e, f, g, h, i, j) = pack $
  ( pack a
  , pack b
  , pack c
  , pack d
  , pack e
  , pack f
  , pack g
  , pack h
  , pack i
  , pack j
  )
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput =
      stimuliGenerator
        clk
        rst
        (( FooProduct (1 :: Unsigned 2) (2 :: Unsigned 2)
         , FooSumA
         , FooSumB
         , FooSumC
         , FooSumG
         , FooSP1_AB (1 :: Unsigned 3) (2 :: Unsigned 5)
         , FooSP1_BA (2 :: Unsigned 5) (1 :: Unsigned 3)
         , FooSP2_AB (2 :: Unsigned 3) (1 :: Unsigned 5)
         , FooSP2_A (2 :: Unsigned 3)
         , FooSP2_B (1 :: Unsigned 5)
         ) :> Nil)
    expectedOutput =
      outputVerifierBitVector
        clk
        rst
        (pack ( $(lift (pack (FooProduct (1 :: Unsigned 2) (2 :: Unsigned 2))))
              , $(lift (pack FooSumA))
              , $(lift (pack FooSumB))
              , $(lift (pack FooSumC))
              , $(lift (pack FooSumG))
              , $(lift (pack (FooSP1_AB (1 :: Unsigned 3) (2 :: Unsigned 5))))
              , $(lift (pack (FooSP1_BA (2 :: Unsigned 5) (1 :: Unsigned 3))))

              , $(lift (pack (FooSP2_AB (2 :: Unsigned 3) (1 :: Unsigned 5))))
              , $(lift (pack (FooSP2_A (2 :: Unsigned 3) :: FooSP2 (Unsigned 3) (Unsigned 5))))
              , $(lift (pack (FooSP2_B (1 :: Unsigned 5) :: FooSP2 (Unsigned 3) (Unsigned 5))))
              ) :> Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
