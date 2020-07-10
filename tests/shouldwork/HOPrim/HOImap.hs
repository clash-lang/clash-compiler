{-# LANGUAGE ScopedTypeVariables #-}

module HOImap where

import Clash.Prelude
import Clash.Explicit.Testbench

-- apply input signal on function matching address
-- other functions gets input nothing
-- Does sythesize
busSwitch2
  :: forall n dom a b
   . KnownNat n
  => Vec n (Signal dom (Maybe a) -> Signal dom b ) -- vector of functions
  -> Signal dom (SatIndex 'SatError n)             -- address as index of vectors
  -> Signal dom (Maybe a)               -- input
  -> Vec n (Signal dom b)                      -- output
busSwitch2 vec addr inp = zipWith ($) vec r where
    r = unbundle (liftA2 f addr inp)
    f i a = replace i a (repeat Nothing)

-- returns output from function where
-- address applies
-- other functions gets input nothing
-- Does not sythesize
busSwitch1
  :: forall n dom a b
   . KnownNat n
  => Vec n (Signal dom (Maybe a) -> Signal dom b ) -- vector of functions
  -> Signal dom (SatIndex 'SatError n)                      -- address as index of vectors
  -> Signal dom (Maybe a)                      -- input
  -> Vec n (Signal dom b)                      -- output
busSwitch1 vec addr inp = r where
    r = imap f vec
    f :: SatIndex 'SatError n
      -> (Signal dom (Maybe a) -> Signal dom b)
      -> Signal dom b
    f n x = x s where
       s :: Signal dom (Maybe a)
       s = liftA2 fa inp addr
       fa :: Maybe a
          -> SatIndex 'SatError n
          -> Maybe a
       fa i a | n == a    = i
              | otherwise = Nothing

-- based on address modify input signal
topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> (Signal System (SatIndex 'SatError 4)
     ,Signal System (Maybe (Signed 5)))
  -> Signal System (Vec 4 (Maybe (Signed 5)))
topEntity = exposeClockResetEnable go where
  go (i,s) = bundle $ busSwitch1 v i s where
    v = f (+1)         -- if address == 0 increment 1
     :> f (*2)         -- if address == 1 multiply 2
     :> f (subtract 1) -- if address == 2 subtract
     :> f negate       -- if address == 3 negate
     :> Nil
    f xK = fmap (fmap xK)
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst $(listToVecTH ([(a,Just b) | b <- [1,2,3,4],  a <- [0,1,2,3]] :: [(Index 4, Maybe (Signed 5))]))
    expectedOutput = outputVerifier'   clk rst $(listToVecTH [$(listToVecTH [Just 2 :: Maybe (Signed 5),Nothing,Nothing,Nothing])
                                                ,$(listToVecTH [Nothing :: Maybe (Signed 5),Just 2,Nothing,Nothing])
                                                ,$(listToVecTH [Nothing :: Maybe (Signed 5),Nothing,Just 0,Nothing])
                                                ,$(listToVecTH [Nothing :: Maybe (Signed 5),Nothing,Nothing,Just (-1)])
                                                ])
    done           = expectedOutput (topEntity clk rst enableGen (unbundle testInput))
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
