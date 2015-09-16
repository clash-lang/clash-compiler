module Bounds where
import CLaSH.Prelude

(expected,actual) = unzip $
        (   0, toInteger (minBound :: Index 43))
     :> (  42, toInteger (maxBound :: Index 43))
     :> (-256, toInteger (minBound :: Signed 9))
     :> ( 255, toInteger (maxBound :: Signed 9))
     :> (   0, toInteger (minBound :: Unsigned 9))
     :> ( 511, toInteger (maxBound :: Unsigned 9))
     :> ( -64, toInteger $ unFixed (minBound :: SFixed 4 3))
     :> (  63, toInteger $ unFixed (maxBound :: SFixed 4 3))
     :> (   0, toInteger $ unFixed (minBound :: UFixed 3 4))
     :> ( 127, toInteger $ unFixed (maxBound :: UFixed 3 4))
     :> Nil

topEntity :: Signal () -> Signal Integer
topEntity = mealy loop actual

loop :: Vec (n+2) a -> () -> (Vec (n+2) a, a)
--loop (x:>xs) _ = (xs <: last xs, x)
loop xs _ = (xs <<+ last xs, head xs)


expectedOutput :: Signal Integer -> Signal Bool
expectedOutput = outputVerifier expected

-- create the right number of inputs
stimuli = map (const ()) actual

testInput = stimuliGenerator stimuli
