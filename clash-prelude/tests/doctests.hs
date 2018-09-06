{-# LANGUAGE CPP #-}
module Main where

import Test.DocTest (doctest)

main :: IO ()
main = doctest (docTestOpts ++ ["-isrc","src/Clash/Prelude.hs"]) >>
       doctest (docTestOpts ++ ["src/Clash/Tutorial.hs"]) >>
       doctest (docTestOpts ++ ["src/Clash/Examples.hs"])

docTestOpts :: [String]
docTestOpts =
#if __GLASGOW_HASKELL__ >= 802
  ["-fdiagnostics-color=never"]
#else
  []
#endif
