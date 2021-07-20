{-# LANGUAGE CPP #-}

module T431 where

import qualified Prelude as P

import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory)

import Clash.Prelude

data TrafficLight = Red | RedAmber | Amber | Green

topEntity :: TrafficLight -> TrafficLight
topEntity Red      = RedAmber
topEntity RedAmber = Green
topEntity Amber    = Red
topEntity Green    = Amber

assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise = P.error $ mconcat [ "Expected:\n\n  ", needle
                                  , "\n\nIn:\n\n", haystack ]

assertNotIn :: String -> String -> IO ()
assertNotIn needle haystack
  | needle `isInfixOf` haystack = P.error $ mconcat [ "Unexpected:\n\n  ", needle
                                                    , "\n\nIn:\n\n", haystack ]
  | otherwise = pure ()

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.vhdl")

  -- no bit vector literals appear, so no double quotes appear
  assertNotIn "\"" content

  -- enum variants appear in the design
  assertIn "TrafficLight'(Red)" content
  assertIn "TrafficLight'(RedAmber)" content
  assertIn "TrafficLight'(Amber)" content
  assertIn "TrafficLight'(Green)" content
