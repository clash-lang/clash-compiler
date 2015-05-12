module MatrixVect where

import CLaSH.Prelude
import qualified Data.List as L

row1 = 1 :> 2 :> 3 :> Nil
row2 = 4 :> 5 :> 6 :> Nil
row3 = 7 :> 8 :> 9 :> Nil

matrix = row1 :> row2 :> row3 :> Nil
vector = 2 :> 3 :> 4 :> Nil

dotProduct xs ys = foldr (+) 0 (zipWith (*) xs ys)
matrixVector m v = map (`dotProduct` v) m

topEntity :: Vec 3 (Signal (Signed 16)) -> Vec 3 (Signal (Signed 16))
topEntity = (\s i -> ((),matrixVector matrix i)) <^> ()

testInput :: Signal (Vec 3 (Signed 16))
testInput = stimuliGenerator ((2 :> 3 :> 4 :> Nil) :> Nil)

expectedOutput :: Signal (Vec 3 (Signed 16)) -> Signal Bool
expectedOutput = outputVerifier ((20 :> 47 :> 74 :> Nil) :> Nil)
