module BoxGrow where

import CLaSH.Prelude

ys >:> xss = zipWith (:>) ys xss
xss <:< ys = zipWith (<:) xss ys

box0 :: Vec 5 (Vec 6 Bit) -> Vec 7 (Vec 8 Bit)
box0 grid =     (zeroesM :> ((zeroesN >:> grid) <:< zeroesN)) <: zeroesM
        where
          zeroesN = replicate d5 0
          zeroesM = replicate d8 0

topEntity = box0

testInput :: Signal (Vec 5 (Vec 6 Bit))
testInput = signal (repeat (repeat 1))

expectedOutput :: Signal (Vec 7 (Vec 8 Bit)) -> Signal Bool
expectedOutput = outputVerifier ( ((0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> Nil) :>
                                  (0 :> 1 :> 1 :> 1 :> 1 :> 1 :> 1 :> 0 :> Nil) :>
                                  (0 :> 1 :> 1 :> 1 :> 1 :> 1 :> 1 :> 0 :> Nil) :>
                                  (0 :> 1 :> 1 :> 1 :> 1 :> 1 :> 1 :> 0 :> Nil) :>
                                  (0 :> 1 :> 1 :> 1 :> 1 :> 1 :> 1 :> 0 :> Nil) :>
                                  (0 :> 1 :> 1 :> 1 :> 1 :> 1 :> 1 :> 0 :> Nil) :>
                                  (0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> Nil) :> Nil) :> Nil)
