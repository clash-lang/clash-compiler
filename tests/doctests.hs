module Main where

import Test.DocTest (doctest)

main :: IO ()
main = doctest ["-i src","CLaSH.Prelude","CLaSH.Examples","CLaSH.Tutorial"]
