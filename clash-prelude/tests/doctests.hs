{-# LANGUAGE CPP #-}
module Main where

import Test.DocTest (mainFromCabal)
import System.Environment (getArgs)

main :: IO ()
main = mainFromCabal "clash-prelude" =<< fmap (extraArgs ++) getArgs

extraArgs :: [String]
extraArgs = map ("--ghc-arg=" ++)
  [
#if __GLASGOW_HASKELL__ >= 904
    "-DCLASH_OPAQUE=OPAQUE"
#else
    "-DCLASH_OPAQUE=NOINLINE"
#endif
#ifdef CLASH_MULTIPLE_HIDDEN
  , "-DCLASH_MULTIPLE_HIDDEN"
#endif
  ]
