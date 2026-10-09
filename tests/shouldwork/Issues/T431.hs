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

-- Enums were requested in #431 but have since been removed again. Guard that
-- sum types stay rendered as bit vectors.
mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.vhdl")

  -- no enum variants appear in the design
  assertNotIn "TrafficLight'(Red)" content
  assertNotIn "TrafficLight'(RedAmber)" content
  assertNotIn "TrafficLight'(Amber)" content
  assertNotIn "TrafficLight'(Green)" content

  -- the constructors are encoded as bit vector literals instead
  assertIn "\"01\" when \"00\"" content
  assertIn "\"11\" when \"01\"" content
  assertIn "\"00\" when \"10\"" content
  assertIn "\"10\" when others" content
