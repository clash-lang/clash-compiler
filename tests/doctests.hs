{-# LANGUAGE CPP #-}
module Main where

import Test.DocTest (doctest)

main :: IO ()
main = doctest $
#if __GLASGOW_HASKELL__ >= 802
  ("-fdiagnostics-color=never":)
#endif
  ["-i src","CLaSH.Prelude","CLaSH.Examples","CLaSH.Tutorial"]
