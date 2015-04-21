module Main where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = glob "src/**/*.hs" >>=
       doctest . (["-optP-include",
                   "-optPdist/build/autogen/cabal_macros.h"] ++)
