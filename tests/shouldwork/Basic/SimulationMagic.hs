module SimulationMagic where

import Prelude as P
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Clash.Prelude
import Clash.Magic (clashSimulation)

f :: Int
f | clashSimulation = 123
  | otherwise       = 456
{-# ANN f (defSyn "f") #-}

assertNotIn :: String -> String -> IO ()
assertNotIn needle haystack
  | P.not (needle `isInfixOf` haystack) = return ()
  | otherwise = P.error $ P.concat [ "Did not expect:\n\n  ", needle
                                   , "\n\nIn:\n\n", haystack ]


assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise = P.error $ P.concat [ "Expected:\n\n  ", needle
                                   , "\n\nIn:\n\n", haystack ]

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  content  <- readFile (topDir </> show 'f </> "f.vhdl")
  assertNotIn "123" content
  assertIn "456" content
