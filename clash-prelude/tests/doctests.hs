{-# LANGUAGE CPP #-}
module Main where

import Test.DocTest (mainFromCabal)
import System.Environment (getArgs)

main :: IO ()
main = mainFromCabal "clash-prelude" =<< fmap (extraArgs ++) getArgs

extraArgs :: [String]
extraArgs = map ("--ghc-arg=" ++)
  [
    "-DCLASH_OPAQUE=OPAQUE"
  ]
