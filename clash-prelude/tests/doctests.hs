{-# LANGUAGE CPP #-}
module Main where

import Test.DocTest (mainFromCabal)
import System.Environment (getArgs)

main :: IO ()
main = mainFromCabal "clash-prelude" =<< getArgs
