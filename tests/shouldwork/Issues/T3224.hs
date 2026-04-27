{-# OPTIONS_GHC -O0 #-}

module T3224 where

import qualified Prelude as P

import Clash.Prelude
import Clash.Annotations.SynthesisAttributes

import System.Environment (getArgs)
import System.FilePath ((</>))

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector 8) ->
  Signal System (BitVector 8) `Annotate` StringAttr "foo" "B2"
topEntity clk rst ena i = leds
 where
  leds = liftA2 xor i regOut
  regOut = withClockResetEnable clk rst ena $ register 0 leds
{-# OPAQUE topEntity #-}

countOccurrences :: String -> String -> Int
countOccurrences needle = go
 where
  n = P.length needle
  go [] = 0
  go s@(_:rest)
    | needle == P.take n s = 1 + go rest
    | otherwise = go rest

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  content <- P.readFile (topDir </> show 'topEntity </> "topEntity.vhdl")

  let n = countOccurrences "\"B2\"" content
  if n == 1
    then return ()
    else P.error (P.concat ["Expected exactly 1 occurrence of \"B2\", found ", P.show n, ":\n\n", content])
