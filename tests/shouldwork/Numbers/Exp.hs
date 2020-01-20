{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=15 #-}

module Exp (testInput, topEntity, expectedOutputs, packedExpectedOutputs) where

import Clash.Prelude
import ConstantFoldingUtil (lit)


testInput :: Vec _ Integer
testInput =
     lit 0
  :> lit 1
  :> lit 2
  :> lit 22101  -- for constant folding
  :> Nil


topEntity :: Integer -> _
topEntity b =
  ( -- We can only test really small exponents: VHDL only supports
    -- exponentiations up to 31 bits, and we need a large base (22101) to
    -- check constant folding.
    resize ((fromInteger b :: Signed 64) ^ d2) :: Signed 64
  , resize ((fromInteger b :: Unsigned 64) ^ d2) :: Unsigned 64
  , resize ((fromInteger b :: Index (2^64)) ^ d2) :: Index (2^64)
  )
{-# NOINLINE topEntity #-}


-- Should be constant folded, and yield the same results as topEntity
expectedOutputs =
     topEntity (testInput !! 0)
  :> topEntity (testInput !! 1)
  :> topEntity (testInput !! 2)
  :> topEntity (testInput !! 3)
  :> Nil

packedExpectedOutputs :: Vec 4 (BitVector 192)
packedExpectedOutputs =
     pack (topEntity (testInput !! 0))
  :> pack (topEntity (testInput !! 1))
  :> pack (topEntity (testInput !! 2))
  :> pack (topEntity (testInput !! 3))
  :> Nil
