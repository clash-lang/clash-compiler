module CharTest where

import CLaSH.Prelude
import Data.Char

topEntity :: (Int,Char) -> (Int,Char,Char)
topEntity (i,c) = (ord c,chr i,'λ')

testInput :: Signal (Int,Char)
testInput = stimuliGenerator $(v [(0::Int,'a'),(3,' '),(8,'\t'),(50,'8'),(600,'λ')])

expectedOutput :: Signal (Int,Char,Char) -> Signal Bool
expectedOutput = outputVerifier $(v ([(97,'\NUL','\955'),(32,'\ETX','\955'),(9,'\b','\955'),(56,'2','\955'),(955,'\600','\955')]::[(Int,Char,Char)]))
