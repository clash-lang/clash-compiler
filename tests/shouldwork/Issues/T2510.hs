{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module T2510 where

import qualified Prelude as P
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Data.String.Interpolate (__i)
import Clash.Explicit.Prelude
import Clash.Annotations.Primitive

class X a where
  x :: a

instance X (Signal System Int) where
  x = pure 3

instance X a => X (Signal System Int -> a) where
  x !_i = x

bb :: X a => a
bb = x
{-# NOINLINE bb #-}
{-# ANN bb hasBlackBox #-}
{-# ANN bb (
  let bbName = show 'bb
  in InlineYamlPrimitive [minBound..] [__i|
    BlackBox:
      name: "#{bbName}"
      kind: Expression
      template: "this should end up in HDL with OPAQUE~ARG[1]"
|]) #-}

bbWrapper :: X a => a
bbWrapper = bb
{-# NOINLINE bbWrapper #-}

topEntity :: Signal System Int -> Signal System Int
topEntity i = bbWrapper i

assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise                   = P.error $ P.concat [ "Expected:\n\n  ", needle
                                                     , "\n\nIn:\n\n", haystack ]

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "bbWrapper.vhdl")
  assertIn "this should end up in HDL with OPAQUE" content
