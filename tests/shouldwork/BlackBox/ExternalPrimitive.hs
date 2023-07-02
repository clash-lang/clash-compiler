{-# LANGUAGE CPP #-}
{-# LANGUAGE PartialTypeSignatures #-}

module ExternalPrimitive where

import qualified Prelude as P
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Clash.Prelude
import Clash.Annotations.Primitive (hasBlackBox)

jsonPrim :: Int
jsonPrim = 0
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE jsonPrim #-}
{-# ANN jsonPrim hasBlackBox #-}

yamlPrim :: Int
yamlPrim = 1
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE yamlPrim #-}
{-# ANN yamlPrim hasBlackBox #-}

topEntity :: Vec _ Int
topEntity =
     jsonPrim
  :> yamlPrim
  :> Nil

assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise                   = P.error $ P.concat [ "Expected:\n\n  ", needle
                                                     , "\n\nIn:\n\n", haystack ]

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.vhdl")
  assertIn "fekkjruzth-json" content
  assertIn "kod0x1ta1k-yaml" content
