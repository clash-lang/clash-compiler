{-# LANGUAGE TemplateHaskell #-}

module T3434 where

import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving
import Clash.Explicit.Testbench
import Clash.Prelude

data PrefixedBox = PrefixedBox (BitVector 2) (BitVector 2)
  deriving (Generic, NFDataX)

{-# ANN module (DataReprAnn
  $(liftQ [t|PrefixedBox|])
  8
  [ConstrRepr 'PrefixedBox 0b1111_0000 0b0101_0000 [0b0000_1100, 0b0000_0011]]) #-}

$(deriveBitPack [t|PrefixedBox|])

-- Restoring a sample's constructor must restore its fixed representation bits,
-- even though the Signal's stream spine has no HDL representation.
topEntity
  :: Signal System (BitVector 4)
  -> Signal System (BitVector 8, BitVector 4, BitVector 4, BitVector 5)
topEntity input = bundle
  ( pack <$> ensureSpine (box <$> input)
  , prefix (ensureSpine (errorX "undefined stream"))
  , prefix (ensureSpine (pure (errorX "undefined samples")))
  , nestedPrefix <$> ensureSpine (optional <$> input)
  )
 where
  box bits = PrefixedBox (slice d3 d2 bits) (slice d1 d0 bits)

  -- Only observe the constructor bits: the payload remains undefined.
  prefix :: Signal System PrefixedBox -> Signal System (BitVector 4)
  prefix = fmap (slice d7 d4 . pack)

  -- A defined sum tag must be preserved while restoring its nested payload.
  optional bits = if testBit bits 0 then Just (errorX "nested sample") else Nothing
  nestedPrefix :: Maybe PrefixedBox -> BitVector 5
  nestedPrefix Nothing = 0
  nestedPrefix (Just b) = 0b10000 .|. resize (slice d7 d4 (pack b))
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  testInput = stimuliGenerator clk rst
    (0x0 :> 0x1 :> 0x6 :> 0xd :> Nil)
  expectedOutput = outputVerifier' clk rst
    ( (0x50, 0x5, 0x5, 0x00)
    :> (0x51, 0x5, 0x5, 0x15)
    :> (0x56, 0x5, 0x5, 0x00)
    :> (0x5d, 0x5, 0x5, 0x15)
    :> Nil
    )
  done = expectedOutput (topEntity testInput)
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen
{-# OPAQUE testBench #-}
