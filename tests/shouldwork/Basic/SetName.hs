module SetName where

import Prelude as P
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory)

import Clash.Prelude

f :: Int -> Int -> Int
f a b = g a b * g b a
{-# ANN f (Synthesize
    { t_name   = "f"
    , t_inputs = [PortName "f_x", PortName "f_y"]
    , t_output = PortName "result"
    }) #-}

g :: Int -> Int -> Int
g = setName @"foo" h

h :: Int -> Int -> Int
h x y = if signum (x - y) == 1 then x else y
{-# ANN h (Synthesize
    { t_name   = "h"
    , t_inputs = [PortName "x", PortName "y"]
    , t_output = PortName "diff"
    }) #-}

assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise = P.error $ P.concat [ "Expected:\n\n  ", needle
                                   , "\n\nIn:\n\n", haystack ]

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  content  <- readFile (takeDirectory topDir </> "f" </> "f.vhdl")

  assertIn "foo" content
  assertIn "foo_0" content

