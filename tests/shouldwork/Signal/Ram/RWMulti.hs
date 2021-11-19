-- An asyncRam with different read and write clocks
--
-- Test write-port-to-read-port data propagation behavior for clocks with a
-- small coprime factor in the periods. When active edges on both ports
-- coincide, it should exhibit "read old data" behavior.
--
-- The following diagrams show the expected behavior. Time is on the horizontal
-- axis, data is located at the time of the respective active edge. 'w' is the
-- data being written to the RAM and 'r' is the data read from the RAM. Two
-- diagrams are given, depending on which of the two clocks is the slower one.
--
-- Period relation 5-3 / 3-5:
--
-- w 0    1    2    3    4    5    6    7    8    9
-- r X  0  1  1  2  2  3  4  4  5  5  6  7  7  8  8
--
-- w 0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
-- r X    1    3    4    6    8    9    B    D    E

{-# OPTIONS_GHC "-Wno-orphans" #-}

module RWMulti where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

createDomain vSystem{vName="P30", vPeriod=30000}
createDomain vSystem{vName="P50", vPeriod=50000}

ram
  :: forall wdom rdom
   . ( KnownDomain rdom
     , KnownDomain wdom)
  => Clock wdom
  -> Clock rdom
  -> Signal wdom (Unsigned 4)
  -> Signal rdom (Unsigned 4)
ram wClk rClk wrD = asyncRamPow2 wClk rClk enableGen rd wrM
 where
  rd = pure (0 :: Unsigned 1)
  wrM = (\d -> Just (0, d)) <$> wrD

tbOutput
  :: forall wdom rdom
   . ( KnownDomain rdom
     , KnownDomain wdom)
  => (  Clock wdom
      -> Clock rdom
      -> Signal wdom (Unsigned 4)
      -> Signal rdom (Unsigned 4))
  -> Clock wdom
  -> Clock rdom
  -> Signal rdom (Unsigned 4)
tbOutput top wClk rClk = output
 where
  wrD = delay wClk enableGen 0 $ wrD + 1
  output = ignoreFor rClk noReset enableGen d1 0 $ top wClk rClk wrD
  noReset = unsafeFromHighPolarity @rdom (pure False)
{-# INLINE tbOutput #-}

tb
  :: forall wdom rdom n
   . ( KnownDomain rdom
     , KnownDomain wdom
     , KnownNat n
     , 1 <= n)
  => (  Clock wdom
      -> Clock rdom
      -> Signal wdom (Unsigned 4)
      -> Signal rdom (Unsigned 4))
  -> Vec n (Unsigned 4)
  -> Signal rdom Bool
tb top expectedSamples = done
 where
  output = tbOutput top wClk rClk
  expectedOutput = outputVerifier' rClk noReset expectedSamples
  done = expectedOutput output
  (rClk, wClk) = biTbClockGen (not <$> done) :: (Clock rdom, Clock wdom)
  noReset = unsafeFromHighPolarity @rdom (pure False)
{-# INLINE tb #-}
